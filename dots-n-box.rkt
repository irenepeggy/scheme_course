#lang racket
(require racket/class
         racket/set
         lazy/force
         racket/promise
         racket/serialize
         compatibility/mlist
         (only-in racket/list shuffle argmax))



;; Реализация минимакса с альфа-бета отсечением
(define (minimax tree)
  (define (minimax-h node alpha beta max-player)
    (define (next-max x v)
      (if (or (null? x) (<= beta v)) 
                v
                (next-max (cdr x)
                      (max v (minimax-h (car x) v beta (not max-player)))))
    )
    (define (next-min x v)
      (if (or (null? x) (<= v alpha)) 
                v
                (next-min (cdr x)
                      (min v (minimax-h (car x) alpha v (not max-player)))))
    )
    (cond 
         ((number? node) node)
         ((null? node) 0.0)
         (max-player (next-max node alpha))
         (else (next-min node beta)))
          
  )
  (minimax-h tree -inf.0 +inf.0 #f)
 )

; определяет какая линия не проведена в боксе b (предполагается, что в боксе уже 3 линии)
(define (missed-line b)
  (cond ((not (set-member? (lines b) 'l)) 'l)
        ((not (set-member? (lines b) 'r)) 'r)
        ((not (set-member? (lines b) 'u)) 'u)
        ((not (set-member? (lines b) 'd)) 'd)
    )
  )

; возвращает первый элемент списка lst, удовлетворяющий предикату pred
; если такого нет, то #false
(define (find-first lst pred)
  (cond ((null? lst) #false)
        ((pred (car lst)) (car lst))
        (else (find-first (cdr lst) pred))
        )
  )

; пока есть боксы с 3-мя палочками, проводит 4-ю,
; ставит пробные значки sign и возвращает обновлённое состояние сетки
(define (do-moves S sign)
  (let ((b (find-first matrix
                  (lambda(x) (= (set-count (lines (grid-ref S (car x) (cadr x)))) 3)))))
    (if (false? b) S
        (do-moves (car (update-grid S (car b) (cadr b) (missed-line (grid-ref S (car b) (cadr b))) sign)) sign)
      )
    )
  )

;; функция эвристической оценки позиции
;; разница между набранными очками текущего игрока и его оппонента при текущем состоянии игры
(define (f S player-sign)
  (if (equal? player-sign 'X) (- (qx S) (qo S)) (- (qo S) (qx S)))
  ) 
; игровая ситуация представляется списком боксов,
; каждый из которых соответствует ячейке в сетке  5х5
; бокс представлен набором проведённых в нём линий и помечен значком одного из игроков либо N
(struct grid (boxes qx qo))

; геттер для всех боксов сетки
(define boxes grid-boxes) ;

; геттеры для очков игроков, играющих крстиками и ноликами, соответственно
(define qx grid-qx)
(define qo grid-qo)

; возвращает бокс c координатами (row, col) из текущего состояния сетки  S
(define (grid-ref S row col)
  (if (coord-valid? row col)
      (list-ref (boxes S) (+ (* row 5) col)) #false))

; вычисляет координаты бокса, являющегося соседним по линии line с
; с боксом имеющим координакты (row, col)
;
; возвращает вычисленные координаты
(define (compute-neighbour row col line)
  (case line
    ('l (cons (cons row (- col 1)) 'r))
    ('r (cons (cons row (+ col 1)) 'l))
    ('u (cons (cons (- row 1) col) 'd))
    ('d (cons (cons (+ row 1) col) 'u))
    )
  )
; добавляет в бокс с индексом box-index новую линию и обновляет его значок на sign
; возвращает обновлённое состояние сетки  S
(define (update-box S row col line sign)
  (let* ((box-index (+ (* row 5) col))
         (tail (list-tail (boxes S) box-index))
         (bx (box (set-add (lines (car tail)) line) sign))
         (qx-new (if (equal? sign 'X) (+ (qx S) 1) (qx S)))
         (qo-new (if (equal? sign 'O) (+ (qo S) 1) (qo S))))  
    (grid (append (drop-right (boxes S) (- 25 box-index)) (cons bx (cdr tail))) qx-new qo-new))
  )

; Обновляет состояние сетки S: основной бокс, указанный в ходе, и соседний с ним.

; row, col - номер строки и столбца бокса,
; line - линия в боксе
; player-sign - значок игрока, совершающего этот ход
;
; возвращает обновлённое состояние сетки и true/false, если
; этим ходом игрок закрыл/не закрыл бокс
(define (update-grid S row col line player-sign)
  (let* ((main-box (grid-ref S row col))
         (nb-move (compute-neighbour row col line))
         (nb-box (grid-ref S (car(car nb-move)) (cdr(car nb-move))))
         ; обновление основного бокса
         (main-update
             (update-box S row col line (if (= (set-count (lines main-box)) 3) player-sign 'N)))
         ; если у основного бокса есть соседний по стороне, где провели линию,
         ; то производится побочное обновление
         (nb-update
             (if nb-box
                (update-box main-update (car(car nb-move)) (cdr(car nb-move)) (cdr nb-move) (if (= (set-count (lines nb-box)) 3) player-sign 'N))
                main-update)))
    (cons nb-update
          ; возвращаем  #true, если в основном или соседнем боксе был поставлен значок игрока
          (or (< (qx S) (qx nb-update)) (< (qo S) (qo nb-update))))
    
    )
  )

        
; показывает текущий счёт в игре
(define (print-counts S)
 (printf "X count: ~a, " (qx S))
 (printf " count: ~a \n" (qo S)))

; структура данных для коробки
(struct box (lines sign))

;______ геттеры для полей box_____________________________

; проведённые для коробки линии (set)
; линии принимают значения 'l, 'r, 'u, 'd
(define lines box-lines)

; значок, поставленный в коробке
; 'N - если коробка ещё не полностью обведена,
; иначе player-sign игрока, который её обвёл
(define sign box-sign)
;___________________________________________________________

;_______готовые игровые ситуации____________________________
; пустая сетка с чистыми коробками для начала игры
(define empty-boxes
  (list (box (set) 'N) (box (set) 'N) (box (set) 'N) (box (set) 'N) (box (set) 'N)
        (box (set) 'N) (box (set) 'N) (box (set) 'N) (box (set) 'N) (box (set) 'N)
        (box (set) 'N) (box (set) 'N) (box (set) 'N) (box (set) 'N) (box (set) 'N)
        (box (set) 'N) (box (set) 'N) (box (set) 'N) (box (set) 'N) (box (set) 'N)
        (box (set) 'N) (box (set) 'N) (box (set) 'N) (box (set) 'N) (box (set) 'N)))
; частично заполненная сетка
(define example-boxes
  (list (box (set 'l 'r 'u 'd) 'X) (box (set 'l 'r 'u 'd) 'X) (box (set 'l 'r 'u 'd) 'O) (box (set 'l 'r 'u 'd) 'X) (box (set 'l 'r 'u 'd) 'X)
        (box (set 'l 'r 'u 'd) 'X) (box (set 'l 'r 'u 'd) 'O) (box (set 'u 'l) 'N) (box (set 'r 'u) 'N) (box (set 'l 'r 'u 'd) 'O)
        (box (set 'l 'r 'u 'd) 'X) (box (set 'l 'r 'u 'd) 'X) (box (set 'l) 'N) (box (set 'r) 'N) (box (set 'l 'r 'u 'd) 'O)
        (box (set 'l 'r 'u 'd) 'O) (box (set 'l 'u) 'N) (box (set 'r) 'N) (box (set 'l 'r) 'N) (box (set 'l 'r 'u 'd) 'O)
        (box (set 'l 'r 'u 'd) 'O) (box (set 'l 'd) 'N) (box (set 'd 'r) 'N) (box (set 'r 'l) 'N) (box (set 'l 'r 'u 'd) 'O)))
; практически заполненная сетка
(define example1-boxes
  (list (box (set 'l 'r 'u 'd) 'X) (box (set 'l 'r 'u 'd) 'X) (box (set 'l 'r 'u 'd) 'O) (box (set 'l 'r 'u 'd) 'X) (box (set 'l 'r 'u 'd) 'X)
        (box (set 'l 'r 'u 'd) 'X) (box (set 'l 'r 'u 'd) 'O) (box (set 'l 'r 'u) 'O) (box (set 'l 'r 'u 'd) 'O) (box (set 'l 'r 'u 'd) 'O)
        (box (set 'l 'r 'u 'd) 'X) (box (set 'l 'r 'u 'd) 'X) (box (set 'l 'r ) 'N) (box (set 'l 'r 'u 'd) 'O) (box (set 'l 'r 'u 'd) 'O)
        (box (set 'l 'r 'u 'd) 'O) (box (set 'l 'r 'u 'd) 'X) (box (set 'r 'd 'l) 'N) (box (set 'l 'r 'u 'd) 'X) (box (set 'l 'r 'u 'd) 'O)
        (box (set 'l 'r 'u 'd) 'O) (box (set 'l 'r 'u 'd) 'X) (box (set 'l 'r 'u 'd) 'X) (box (set 'l 'r 'u 'd) 'X) (box (set 'l 'r 'u 'd) 'O)))
;_________________________________________________________________________________

; список координат всех боксов в сетке 5х5
(define matrix
  (list '(0 0) '(0 1) '(0 2) '(0 3) '(0 4)
       '(1 0) '(1 1) '(1 2) '(1 3) '(1 4)
       '(2 0) '(2 1) '(2 2) '(2 3) '(2 4)
       '(3 0) '(3 1) '(3 2) '(3 3) '(3 4)
       '(4 0) '(4 1) '(4 2) '(4 3) '(4 4))
  )
; проверяет, не выходят ли координаты за границы допустимого поля 5х5
; (координаты берутся "компьютерные", то есть  [0, 4]
(define (coord-valid? row col)
  (and (< -1 row)
       (< -1 col)
       (> 5 row)
       (> 5 col))
  )

; проверяет, что ход m ещёне был сделан для состояния сетки  S
(define (move-available? m S)
  (let ((bx (grid-ref S (- (car (car m)) 1) (- (cadr (car m)) 1))))
    (not (set-member? (lines bx) (cadr m)))
    )
  )

; все возможные ходы в текущей игровой ситуации S
(define (possible-moves S)
  ; принимает на вход координаты бокса coord и возвращает возможные для этого бокса ходы
  (define (coord->moves coord)
    (let ((cur-existed-lines (lines (grid-ref S (car coord) (cadr coord)))) ; проведённые в боксе линии
          ; избегаем эквивалентных ходов и будем идти по сетке Г-образными "уголками" 
          (considered-lines (cond
                              ; дополнительно рассматриваем нижнюю границу сетки
                              ((=(car coord) 4) '(l u d)) ;
                              ; и правую границу сетки
                              ((=(cadr coord) 4) '(l u r))
                              (else '(l u)))))
      ; генерируем для бокса список возможных для него ходов
      (filter-map (lambda(x) (and (not (set-member? cur-existed-lines x)) (list (list (car coord) (cadr coord)) x)))
                  considered-lines)))
  ; отображаем координаты каждого бокса в список возможных ходов
  ; и "плющим" получившийся список
  (foldl append '() (map coord->moves matrix))
  )

; ввод хода через консоль c проверкой допустимых значений
; 
; 1) проверяется формат: ((x y) line),
; 2) проверяется, что x и y - координаты квадрата из диапазона [1, 5]
; 3) проверяется значение line - правая (r), левая (l), верхняя (u) или нижняя (d) палочка в нём
; 4) проверяется, что линия ещё не проведена
(define (input-move m S)
            (cond
              ((equal? m 'q) (exit))
              ((and
                (list? m)
                (list?(car m))
                (= (length m) 2)
                (= (length (car m)) 2)
                (set-member? (set 'l 'r 'd 'u) (cadr m))
                (coord-valid? (- (car (car m)) 1) (- (cadr (car m)) 1))
                (move-available? m S))  (list (list (- (car (car m)) 1) (- (cadr (car m)) 1)) (cadr m)))
              (else (input-move (read) S))))

; класс, описывающий правила, по которым совершаются ходы в игре
(define game%
  (class object%
    (super-new)

    
    ; функция, осуществляющая серию ходов
    (define/public (do-put-sign-move S sign)
      (let ((b (find-first matrix
                           (lambda(x) (= (set-count (lines (grid-ref S (car x) (cadr x)))) 3)))))
        (if (false? b) S
            (my-move S (car b) (cadr b) (missed-line (grid-ref S (car b) (cadr b))) sign)
            )
        )
      )
    
    ; проверяет, окончена ли игра
    (define/public (game-is-over? S)
     (empty? (filter (lambda(x) (equal?(sign x) 'N)) (boxes S))))
    
    ; проверяет, победил ли игрок, играющий значком player-sign
    (define/public (my-win? S player-sign)
      (> (length (filter (lambda(x) (equal?(sign x) player-sign)) (boxes S)))
         ( / (length (boxes S)) 2)))

    ;; optimal-move :: State -> Move
    ;; выбор оптимального хода по минимаксу 
    ;; из нескольких оптимальных выбирается один случайно
    (define/public ((optimal-move look-ahead player-sign) unused S)
      (argmax (lambda (m) (minimax (game-tree S m look-ahead player-sign)))
                 (shuffle (possible-moves S))))

    ; строит дерево ходов
    (define (game-tree S m look-ahead player-sign)
      ; походили, оцениваем к чему привел совершённый ход через обновлённое состояние *S
      ; i - номер ветвления
      ; cur-gain - очки, набранные ДО этого хода (разница между вновь захваченными боксами текущего игрока и оппонента)
      ; player-sign - игровой значок игрока, результат хода которого мы сейчас наблюдаем
      (define (new-ply i S* player-sign)
        (cond
          ; что если ход привёл к победе?
          ((game-is-over? S*)
            (if (my-win? S* player-sign) +inf.0 -inf.0) ; в выигрышной позиции оценка = + бесконечность
           )
          ; если исчерпана глубина, возвращаем эвристическую оценку
          ((>= i look-ahead) (f S* player-sign)) ; 
          (else (map (lambda (x) (new-ply (+ 1 i) (car (my-move S* x player-sign)) (opponent-player-sign player-sign)))
                     (possible-moves S*))) ; строим ходы соперника
		))
      ; делаем ход, передаем ход сопернику
     (new-ply 1 (car (my-move S m player-sign)) (opponent-player-sign player-sign))
    )
    
    ; непосредственно осуществляет ход (обновляет состояние игровой сетки)
    ; S - текущее состояние игровой сетки
    ; m - ход игрока в формате ((x y) line)
    ; player-sign - значок, которым ходит текущий игрок
    ;
    ; возвращает пару из обновлённого состояния сетки и #f/#t,
    ; если игрок не закрыл/закрыл этим ходом бокс
    (define/public (my-move S m player-sign)
      (let* ((row (car (car m)))
            (col (cadr (car m))))
        (update-grid S row col (cadr m) player-sign) 
        )
      )
    
    ; обёртка над ходом
    ; S - текущее состояние игровой сетки
    ; move-method - функция, совершающая ход
    ; player-sign - значок, которым ходит текущий игрок
    (define/public (make-move S move-method player-sign)      
      (let* ((m* (move-method '() S)) ; получает значение хода '((x y) l)
             (S-result (my-move S m* player-sign)) 
             (S* (car S-result)) ; обновлённая игровая ситуация
             (result* (cdr S-result))) ; кто следущий ходит? тот же игрок или соперник?
        (cond
          ((game-is-over? S*) (values m* S* (if (my-win? S* player-sign) 'win 'loss)))
          (result* (values m* S* 'continue)) 
          (else            (values m* S* 'next))))
    )
  )
  )

; класс, описывающий игрока
(define player%
  (class game% 
    (super-new)
    
    (inherit make-move
             optimal-move)
    
    (init-field name
                player-sign
                [opponent 'undefined]
                [move-method (optimal-move 4 player-sign)])
    ; ход текущего игрока с определением того, как дальше будет идти игра по её правилам
    (define/public (your-turn S)
      (printf "~a turn: " name)
      (define-values (m S* status) (make-move S move-method player-sign))
      (printf "\n~a makes move ~a\n" name (list (list (+ (car (car m)) 1) (+ (cadr (car m)) 1)) (cadr m)))
      (show-grid S*)
      (print-counts S*)
      (case status
        ['stop (displayln "The game was interrupted.")]
        ['win (printf "~a won the game!" name)]
        ['loss (printf "~a loss the game!" name)]
        ['continue (send this your-turn S*)]
        [else (send opponent your-turn S*)]))
    ))

; значки, которыми  играют игроки
(define player-signs (cons 'X 'O))

; принимает на вход значок текущего игрока my-sign и возвращает значок оппонента
(define (opponent-player-sign my-sign)
  (if (equal? my-sign (car player-signs)) (cdr player-signs) (car player-signs)))

(define user-A 
  (new player%
       [name "User X"]
       [player-sign (car player-signs)]
       [move-method 
        (lambda (a b) (input-move (read) empty-grid))]
		)
 )

(define user-B 
  (new player% 
       [name "User O"]
       [player-sign (cdr player-signs)]
       [move-method 
        (lambda (a b) (input-move (read) empty-grid))]
		)
 )

(define player-A 
  (new player%
       [name "Minimax-player X"]
       [player-sign (car player-signs)]
		)
 )

(define player-B 
  (new player% 
       [name "Minimax-player O"]
       [player-sign (cdr player-signs)]
		)
 )
;__________начальные игровые ситуации_______________
; пустая  игровая ситуация
; вызов: (start-game user-A user-B empty-grid)
(define empty-grid (grid empty-boxes 0 0))

; частично заполненная игровая ситуация
; вызов: (start-game user-A user-B example-grid)
; ИИ: (start-game player-A player-B example-grid)
(define example-grid (grid example-boxes 7 8))

; игровая ситуация, в которой нужно сделать один ход для окончания игры
; вызов: (start-game user-A user-B example1-grid)
(define example1-grid (grid example1-boxes 12 11))
;____________________________________________________


(define (start-game p1 p2 initial-state)
  (show-grid initial-state)
  (print-counts initial-state)
  (set-field! opponent p1 p2)
  (set-field! opponent p2 p1)
  (send p1 your-turn initial-state))
  
; отображение игровой ситуации
(define (show-grid g)
  (let ((bx (boxes g)))
  ; номера столбцов
  (displayln "    1    2    3    4    5   ")
  ; верхняя граница - выводится отдельно
  (printf "  .")
  (for ([j '(0 1 2 3 4)])
    (if (set-member? (lines (list-ref bx j)) 'u) (display " .. .") (display "    .")))
  (display "\n")

  ; а тут выводится всё остальное
  (for ([i '(0 1 2 3 4)])
    ; номер строки
    (printf "~a " (+ i 1))
    ; левая граница - выводится отдельно
    (if (set-member? (lines (list-ref bx (* i 5))) 'l) (display ":") (display " "))

    ; содержимое и нижние палочки ящиков
    (for ([j '(0 1 2 3 4)])
      ; содержимое ящика: пустота или знак того, кто его закрыл 
      (let ((own (sign (list-ref bx (+ (* i 5) j)))))
        (if (eqv? own 'N) (display "    ") (printf " ~a~a " own own)))
      ; правые края ящиков
      (if (set-member? (lines (list-ref bx (+ (* i 5) j))) 'r) (display ":") (display " ")))
    (display "\n")
    
    ; нижние края ящиков
    (printf "  .")
    (for ([j '(0 1 2 3 4)])
      (if (set-member? (lines (list-ref bx (+ (* i 5) j))) 'd) (display " .. .") (display "    .")))
    (display "\n")
  )))

 