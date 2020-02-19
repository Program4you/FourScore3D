#lang racket
(require racket/class)


(define game-field%
  (class object%
    (super-new)

    (define FREE 0) ; свободная клетка
    (define X 1) ; клетка с крестиком
    (define O 2) ; клетка с ноликом
    
    (init-field (cells (make-vector 64 FREE))) ; ячейки игрового поля 4х4x4
    (init-field (all-cells (build-vector 16 (lambda (x) (vector (quotient x 4) (modulo x 4)))))) 
    (init-field (curr-player X)) ; начинает крестик
    (init-field (last-cells null)) ; индекс клетки, в которую был сделан последний ход

    ; перевод координат в индекс клетки
    (define (cell-to-index i j k)
        (+ (* 16 k) (* 4 i) j)
    )
    
    ; вывод поля (4 уровня в строку)
    (define/public (show)
        (for ([i 4])
            (for ([k 4])
                (printf "|")
                (for ([j 4])
                    (cond                      
                      ((= (vector-ref cells (cell-to-index i j k)) X) (printf "X")) ; выводим крестик
                      ((= (vector-ref cells (cell-to-index i j k)) O) (printf "O")) ; выводим нолик
                      (else (printf " ")) ; выводим пустоту
                    )
                    (printf "|")
                )

                (printf "    ") ; отсуп для следующего поля
            )

            (printf "\n")
        )
    )

    (define (get-top-k i j)
        (cond
            ((or (< i 0) (< j 0) (> i 3) (> j 3)) -1)
            ((= (vector-ref cells (cell-to-index i j 0)) FREE) 0)
            ((= (vector-ref cells (cell-to-index i j 1)) FREE) 1)
            ((= (vector-ref cells (cell-to-index i j 2)) FREE) 2)
            ((= (vector-ref cells (cell-to-index i j 3)) FREE) 3)
            (else -1)
        )
    )

    ; проверка, можно ли сделать ход в клетку (i, j, k)
    (define/public (can-move i j)
        (let* ((top-k (get-top-k i j)) (index (cell-to-index i j top-k))) ; запоминаем индекс клетки
            (and (> top-k -1) (<= 0 index 63) (= (vector-ref cells index) FREE)) ; в клетку можно пойти, если она валидна и эта клетка свободна
        )
    )
    
    ; добавление фишки на поле
    (define/public (move i j)
        (set! last-cells (cons (cell-to-index i j (get-top-k i j)) last-cells)) ; обновляем позицию хода
        (vector-set! cells (cell-to-index i j (get-top-k i j)) curr-player) ; проставляем на поле текущего игрока
        (set! curr-player (if (= curr-player X) O X)) ; меняем текущего игрока   
    )

    ; отмена последнего хода
    (define/public (remove-last-move)
        (vector-set! cells (car last-cells) FREE) ; обновляем состояние на поле
        (set! last-cells (cdr last-cells)) ; сдвигаем элементы последних ходов
        (set! curr-player (if (= curr-player X) O X)) ; меняем текущего игрока   
    )

    ; проверка линии из 4 клеток на сответствие игроку с
    (define (check-line? i0 j0 k0 di dj dk c)
        (let loop ((index 0) (i i0) (j j0) (k k0))
            (cond
                ((= index 4) #t) ; все клетки соответствуют заданному символу
                ((not (= (vector-ref cells (cell-to-index i j k)) c)) #f) ; если какой-то символ не соответствует c, то и линия не соответствует
                (else (loop (add1 index) (+ i di) (+ j dj) (+ k dk)))
            )
        )
    )

    ; оценка линии из 4 клеток
    (define (eval-line? i0 j0 k0 di dj dk)
        (let loop ((index 0) (i i0) (j j0) (k k0) (free-cells 0) (opponent-cells 0))
            (cond
                ((= index 4) (vector free-cells (- 4 free-cells opponent-cells) opponent-cells)) ; возвращаем вектор с количеством ячеек
                (else
                 (let* (
                       (cell (vector-ref cells (cell-to-index i j k)))
                       (free (if (= cell FREE) 1 0))
                       (opponent (if (= cell curr-player) 1 0))
                       )
                   (loop (add1 index) (+ i di) (+ j dj) (+ k dk) (+ free-cells free) (+ opponent-cells opponent))
                 )
               )
            )
        )
    )

    (define/public (eval)
      (let* (
            (k (quotient (car last-cells) 16))
            (i (quotient (modulo (car last-cells) 16) 4))
            (j (modulo (modulo (car last-cells) 16) 4)) 
            (lines (vector
                    (eval-line? i 0 k 0 1 0) ; прямая слева-направо
                    (eval-line? 0 j k 1 0 0) ; прямая спереди-назад
                    (eval-line? i j 0 0 0 1) ; прямая сверху-вниз

                    (eval-line? i 0 0 0 1 1)
                    (eval-line? i 0 3 0 1 -1)

                    (eval-line? 0 j 0 1 0 1)
                    (eval-line? 0 j 3 1 0 -1)

                    (eval-line? 0 0 k 1 1 0)
                    (eval-line? 0 3 k 1 -1 0)

                    (eval-line? 0 0 0 1 1 1)
                    (eval-line? 0 0 3 1 1 -1)
                    (eval-line? 3 0 0 -1 1 1)
                    (eval-line? 3 0 3 -1 1 -1)
              ))
            )
            (let loop ((i 0) (score 0))
              (if (= i (vector-length lines))
                  score
                  (let* ((line (vector-ref lines i)) (free (vector-ref line 0)) (curr (vector-ref line 1)) (opponent (vector-ref line 2)))
                    (loop (add1 i) (+ score free curr (- opponent)))
                  )
              )            
            )
        )
    )

    ; проверка на ничью
    (define/public (draw?)
        (equal? (vector-member FREE cells) #f) ; если нет ни одной свободной клетки
    )
    
    ; проверка на выигрыш
    (define/public (is-win? last-player)
        (if (null? last-cells) ; если хода ещё не было
            #f ; выигрыша быть не может
            (let ((k (quotient (car last-cells) 16)) (i (quotient (modulo (car last-cells) 16) 4)) (j (modulo (modulo (car last-cells) 16) 4))) 
                (or
                    (check-line? i 0 k 0 1 0 last-player) ; прямая слева-направо
                    (check-line? 0 j k 1 0 0 last-player) ; прямая спереди-назад
                    (check-line? i j 0 0 0 1 last-player) ; прямая сверху-вниз

                    (check-line? i 0 0 0 1 1 last-player)
                    (check-line? i 0 3 0 1 -1 last-player)

                    (check-line? 0 j 0 1 0 1 last-player)
                    (check-line? 0 j 3 1 0 -1 last-player)

                    (check-line? 0 0 k 1 1 0 last-player)
                    (check-line? 0 3 k 1 -1 0 last-player)

                    (check-line? 0 0 0 1 1 1 last-player)
                    (check-line? 0 0 3 1 1 -1 last-player)
                    (check-line? 3 0 0 -1 1 1 last-player)
                    (check-line? 3 0 3 -1 1 -1 last-player)
                )
            )
        )
    )

    (define/public (win?)
      (is-win? (if (= curr-player X) O X))
    )

    (define/public (win-opponent?)
      (vector-set! cells (car last-cells) curr-player)
      (let ((res (is-win? curr-player)))
        (begin
            (vector-set! cells (car last-cells) (if (= curr-player X) O X))
            res
        )
      )
    )

    ; получение последнего игрока
    (define/public (get-last)
        (if (= curr-player X) "O" "X")
    )

    ; получение следующего игрока
    (define/public (get-next)
        (if (= curr-player X) "X" "O")
    )

    ; получение доступных ходов
    (define/public (get-available-moves)
        (vector-filter (lambda (x) (can-move (vector-ref x 0) (vector-ref x 1))) all-cells)
    )
  )
)

; игрок
(define player%
  (class object%
    (super-new)
    
    ; ход игрока
    (define/public (make-move field)
        (let* (
               (i (read))
               (j (read))
            )
          
            (if (send field can-move (- 3 i) j)
                (vector (- 3 i) j)
                (begin (printf "Invalid move, try again\n>") (make-move field))
            )
        )      
    )
  )
)

; случайный игрок
(define random-player%
  (class object%
    (super-new)

    ; ход игрока
    (define/public (make-move field)
        (let* (
               (moves (send field get-available-moves))
               (move (vector-ref moves (random (vector-length moves)))); выбираем случайный из доступных ходов
               ) ; получаем доступые ходы
            (printf "~a ~a\n" (- 3 (vector-ref move 0)) (vector-ref move 1))
            move
        )
    )
  )
)

; игрок, использующий оценочную функцию и minimax алгоритм
(define minimax-player%
  (class object%
    (super-new)

    (define max-depth 3)

    (define (score-function field)
      (let (
            (curr-score (if (send field win?) -100001 0))
            (next-score (if (send field win-opponent?) 100000 0))
           )
        (+ (- curr-score next-score) (send field eval))
      )
    )

    (define (alpha-beta field depth alpha beta player)
      (cond
        [(= depth 0) (- (score-function field))]
        (else
            (let* (
                (score-best beta) ; лучшая оценка
                (score 0) ; переменная для поиска
                (moves (send field get-available-moves)) ; получаем доступные ходы
                (moves-count (vector-length moves)) ; количество доступных ходов
                )

              (let loop ((i 0))
                (cond
                  [(= i moves-count) score-best] ; прошли все доступные позиции или вышли по отсечению
                  (else (begin
                       (send field move (vector-ref (vector-ref moves i) 0) (vector-ref (vector-ref moves i) 1)) ; делаем ход
                       (set! score (- (alpha-beta field (- depth 1) (- score-best) (- alpha) (- player)))) ; оцениваем на ветвь шлубже
                       (send field remove-last-move) ; отменяем ход
                       (set! score-best (min score-best score)) ; обновляем максимальный скор
                       (loop (if (<= score-best alpha) moves-count (add1 i))) ; выходим или не выходим по отсечению       
                    )
                 )     
               )
            )
          )
        )
      )
    )
    
    ; выполнение хода игрока
    (define/public (make-move field)
        (let* (
               (moves (send field get-available-moves)) ; получаем доступные ходы
               (n (vector-length moves)) ; количество доступных ходов
               (scores (make-vector n 0)) ; вектор для очков
             )
             (for ([i n])
                 (send field move (vector-ref (vector-ref moves i) 0) (vector-ref (vector-ref moves i) 1)) ; делаем ход
                 (vector-set! scores i (alpha-beta field max-depth -inf.0 +inf.0 1)) ; оцениваем
                 (send field remove-last-move) ; отменяем
             )

             (let ((best-move (get-best-move moves scores))) ; ищем лучший ход
               (printf "~a ~a\n" (- 3 (vector-ref best-move 0)) (vector-ref best-move 1))
               ;(printf "scores: ~a, moves: ~a\n" scores moves)
               best-move ; возвращаем лучший ход
             )
             
        )
    )

    (define (get-best-move moves scores)
      (let* (
             (pairs (vector-map (lambda (x y) (vector x y)) moves scores)) ; формируем вектор [ход-оценка]
             (vmax (vector-argmax (lambda (x) (vector-ref x 1)) pairs)) ; ищем пару с максимальной оценкой
             (best-score (vector-ref vmax 1)) ; максимальная оценка
             (best-moves (vector-filter (lambda (x) (= (vector-ref x 1) best-score)) pairs)) ; ищем все ходы с максимальной оценкой
             (move (vector-ref best-moves (random (vector-length best-moves)))) ; выбираем случайно ход из найденных
             )

        (vector-ref  move 0) ; возвращаем ход
      )
    )
  )
)

(define game%
  (class object%
    (init player-x) ; крестики
    (init player-o) ; нолики    
    (super-new)
    
    (init-field (field (new game-field%))) ; создаём игровое поле

    ; считывание хода игрока
    (define (make-move player field)     
        (let* (
                   (move (send player make-move field)) ; просим игрока сделать ход
                   (i (vector-ref move 0))
                   (j (vector-ref move 1))
              )
            (send field move i j) ; делаем ход
        )      
    )

    (define (game-loop player-x?) 
      (cond
        ((send field win?) (printf "~a WIN" (send field get-last))) ; выигрыш
        ((send field draw?) (printf "Draw!\n")) ; ничья
        (else
          (printf "~a move>" (send field get-next))
          (make-move (if player-x? player-x player-o) field) ; ход
          (send field show) ; отрисовываем новое поле
          (game-loop (not player-x?)) ; запускаем для противоположного игрока
         )
      )        
    )

    ; запуск игры
    (define/public (start)
        (send field show) ; отрисовываем поле      
        (game-loop #t)
    )
  )
)

(define player-x (new player%))
;(define player-o (new player%))
;(define player-x (new random-player%))
;(define player-o (new random-player%))
;(define player-x (new minimax-player%))
(define player-o (new minimax-player%))
(define game (new game% [player-x player-x] [player-o player-o]))

(send game start)

;(define board (new game-field%)
;(send board show)
;(send board move 0 0)
;(send board move 0 1)
;(send board move 0 2)
;(send board move 2 0)
;(send board move 3 1)
;(send board move 2 1)
;(send board show)
;(printf "\n")
;(send board remove-last-move)
;(send board remove-last-move)
;(send board remove-last-move)
;(send board remove-last-move)
;(send board show)