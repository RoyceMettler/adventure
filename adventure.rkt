;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname adventure) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "adventure-define-struct.rkt")
(require "macros.rkt")
(require "utilities.rkt")
(require racket/format)
(require racket/string)


;;;
;;; OBJECT
;;; Base type for all in-game objects
;;;

(define-struct object
  ;; adjectives: (listof string)
  ;; List of adjectives to be printed in the description of this object
  (adjectives

  ;; yours: boolean
  ;; Boolean to determine whether or not it is yours
  yours?

  ;; the?: boolean
  ;; Boolean to determine whether or not it should use "the" rather than a or an
  the?

  ;; specific: boolean
  ;; Whether or not this object should print a specified description when examined
  specific?


  ;; examine-text: string
  ;; Text to print if the player examines this object
  examine-text
  )
  
  #:methods

  ;; examine: object -> string
  ;; T
  (define (examine o)
    (if (object-specific? o)
        (textboxonlist (append (list (string-append (capital-description o) ".")) (substrings (object-examine-text o))))
        (textbox (capital-description o))))


  ;; noun: object -> string
  ;; Returns the noun to use to describe this object.
  (define (noun o)
    (type-name-string o))

  ;; description-word-list: object -> (listof string)
  ;; The description of the object as a list of individual
  ;; words, e.g. '("a" "red" "door").
  (define (description-word-list o)
    (if (object-yours? o)
        (add-your (append (object-adjectives o)
                          (list (noun o))))
        (if (object-the? o)
            (add-the (append (object-adjectives o)
                             (list (noun o))))
            (add-a-or-an (append (object-adjectives o)
                         (list (noun o)))))))
  
  ;; description: object -> string
  ;; Generates a description of the object as a noun phrase, e.g. "a red door".
  (define (description o)
    (words->string (description-word-list o)))

  ;; descriptionthe: object -> string
  ;; Generates a description of the object as a noun phrase, e.g. "the red door".
  (define (descriptionthe o)
    (if (object-yours? o)
        (words->string (description-word-list o))
        (string-append "the " (words->string (rest (description-word-list o))))))

  ;; lookdesc object -> string
  ;; Generates the text "You are in ___." with ___ being the description of the object
  ;; I moved this here rather than in (look) in the process of adding text boxes to the text

  (define (lookdesc o)
    (string-append "You are in " (string-append (descriptionthe o) ".")))

  ;; golookdesc object -> string
  ;; Generates the text "You go to ___." with ___ being the description of the object
  ;; I moved this here rather than in (look) in the process of adding text boxes to the text

  (define (golookdesc o)
    (string-append "You go to " (string-append (descriptionthe o) ".")))

  
  ;; capital-description: object -> string
  ;; Generates a description of the object as a noun phrase but capitalizes the first word
  (define (capital-description o)
    (if (object-yours? o)
        (string-append "Y" (substring (description o) 1))
        (if (object-the? o)
            (string-append "T" (substring (description o) 1))
            (string-append "A" (substring (description o) 1)))))

  
  ;; print-description: object -> void
  ;; EFFECT: Prints the description of the object.
  (define (print-description o)
    (begin (printf (description o))
           (newline)
           (void)))

  )

;;;
;;; CONTAINER
;;; Base type for all game objects that can hold things
;;;

(define-struct (container object)
  ;; contents: (listof thing)
  ;; List of things presently in this container
  (contents)
  
  #:methods
  ;; container-accessible-contents: container -> (listof thing)
  ;; Returns the objects from the container that would be accessible to the player.
  ;; By default, this is all the objects.  But if you want to implement locked boxes,
  ;; rooms without light, etc., you can redefine this to withhold the contents under
  ;; whatever conditions you like.
  (define (container-accessible-contents c)
    (container-contents c))
  
  ;; prepare-to-remove!: container thing -> void
  ;; Called by move when preparing to move thing out of
  ;; this container.  Normally, this does nothing, but
  ;; if you want to prevent the object from being moved,
  ;; you can throw an exception here.
  (define (prepare-to-remove! container thing)
    (begin
      ;; Desk and bed can't be moved because they are too heavy
      (when (or (string-suffix? (description thing) "desk")
              (string-suffix? (description thing) "bed"))
             (error "It's too heavy."))
      ;; Taking the bookshelf breaks it down into planks
      (when (string-suffix? (description thing) "bookshelf")
        (set-object-examine-text! thing "It's just a bunch of planks now."))
           (void)))
  
  ;; prepare-to-add!: container thing -> void
  ;; Called by move when preparing to move thing into
  ;; this container.  Normally, this does nothing, but
  ;; if you want to prevent the object from being moved,
  ;; you can throw an exception here.
  (define (prepare-to-add! container thing)
    (when (box? container)
      (begin
      ; Coatrack only accepts coats
      (when (string=? (box-noun-to-print container) "coatrack")
        (unless (string=? (prop-noun-to-print thing) "coat")
          (error "You can only put coats on the coatrack!")))
      ; Can't put stuff on bookshelf
      (when (string=? (box-noun-to-print container) "bookshelf")
        (when (have? container)
        (error "You can't use your bookshelf as a backpack!")))
      (when (string-suffix? (description thing) "crowd")
        (error "The crowd is not your personal backpack!"))
      (void)

    )))
  
  
  ;; remove!: container thing -> void
  ;; EFFECT: removes the thing from the container
  (define (remove! container thing)
    (set-container-contents! container
                             (remove thing
                                     (container-contents container))))
  
  ;; add!: container thing -> void
  ;; EFFECT: adds the thing to the container.  Does not update the thing's location.
  (define (add! container thing)
    (set-container-contents! container
                             (cons thing
                                   (container-contents container))))

  ;; describe-contents: container -> list
  ;; Generates a list of the stuff in a container
  (define (describe-contents container)
    (local [(define other-stuff (remove me (container-accessible-contents container)))]
             (if (empty? other-stuff)
                 "There's nothing here."
                 (map (λ (o) (capital-description o)) other-stuff))))

  
  ;; fancy-describe-contents: container -> list
  ;; Generates a list of the stuff in a container, capitalizing the first word
  ;; of each item and adding stylistic dashes to the side of each one
  (define (fancy-describe-contents container)
    (local [(define other-stuff (remove me (container-accessible-contents container)))]
             (if (empty? other-stuff)
                 "There's nothing here."
                 (map (λ (o) (string-append o " -")) (map (λ (o) (string-append "- " o)) (map (λ (o) (capital-description o)) other-stuff))))))

  ;; contents: container -> void
  ;; Prints the contents of a container, unless it is a closed chest then throws an exception.
  (define (contents c)
    (local [(define (inoron inp)
              (if (or (string-suffix? (description inp) "coatrack")
                      (string-suffix? (description inp) "bookshelf")
                      (string-suffix? (description inp) "desk")
                      (string-suffix? (description inp) "bed")
                      (string-suffix? (description inp) "table"))
                  "on "
                  "in "))]
      (if (canyousee? (here))
          (begin (when (or (string-suffix? (description c) "letter-opener")
                       (string-suffix? (description c) "key")
                       (string-suffix? (description c) "coat")
                       (string-suffix? (description c) "flashlight")
                       (string-suffix? (description c) "hay")
                       (string-suffix? (description c) "silver-dollar")
                       (string-suffix? (description c) "clothes")
                       (string-suffix? (description c) "corpse")
                       (npc? c)
                       (string-suffix? (description c) "lighter")
                       (string-suffix? (description c) "coins")
                       (string-suffix? (description c) "trophy"))
               (error "You can't store stuff in there."))
             (when (string-suffix? (description c) "door")
               (error "That's, uh, a door."))
             (when (string-suffix? (description c) "chest")
               (when (not (chest-open? c))
                 (error "That chest is closed. You can't see inside.")))
             (if (empty? (container-accessible-contents c))
                 (textbox (string-append (string-append (string-append "There's nothing " (inoron c)) (descriptionthe c)) "."))
                 (textboxonlist (cons (string-append (string-append "Contents of " (descriptionthe c)) ":") (fancy-describe-contents c))))
             (void))
          (begin (textbox "How could you check contents when you can't see.")
                 (void)))
      ))

  )

;; move!: thing container -> void
;; Moves thing from its previous location to container.
;; EFFECT: updates location field of thing and contents
;; fields of both the new and old containers.
(define (move! thing new-container)
  (begin
    (prepare-to-remove! (thing-location thing)
                        thing)
    (prepare-to-add! new-container thing)
    (prepare-to-move! thing new-container)
    (remove! (thing-location thing)
             thing)
    (add! new-container thing)
    (set-thing-location! thing new-container)))

;; destroy!: thing -> void
;; EFFECT: removes thing from the game completely.
(define (destroy! thing)
  ; We just remove it from its current location
  ; without adding it anyplace else.
  (remove! (thing-location thing)
           thing))

;;;
;;; ROOM
;;; Base type for rooms and outdoor areas
;;;

(define-struct (room container)

  ;; Room-noun-to-print: string
  ;; The noun that this room is called so it isn't just called "room"
  (noun-to-print

   ;; go-text: string
   ;; Text to display when you go to this room
   go-text

   ;; dark?: boolean
   ;; Whether or not this room is dark, meaning it requires a flashlight
   ;; to see in this room
   dark?
   )

  #:methods
  ;; noun-to-print: room -> string
   ;; The user can set the noun to print in the description so it doesn't always end in "room".
  (define (noun room)
    (room-noun-to-print room))

  ;; canyousee?: room -> boolean
  ;; Whether or not you can see in this room. If it's a dark room, you need a lit flashlight.
  (define (canyousee? room)
    (if (room-dark? room)
      (if (ormap (λ (element) (flashlight? element)) (my-inventory))
        (if (flashlight-on? (the flashlight))
          #t
          #f)
        #f)
      #t))
        
  
  )


;; new-room: string -> room
;; Makes a new room with the specified adjectives
(define (new-room description yours? the? go-text dark?)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))]
  (make-room adjectives
             yours? the? #f "" '()
             noun
             go-text
             dark?)))

;;;
;;; THING
;;; Base type for all physical objects that can be inside other objects such as rooms
;;;

(define-struct (thing container)
  ;; location: container
  ;; What room or other container this thing is presently located in.
  (location

  ;; take-text: string
  ;; text to print when you take this item.
   take-text
  ;; drop-text: string
  ;; text to print when you drop this item.
   drop-text 
   )
  
  #:methods
  
  ;; prepare-to-move!: thing container -> void
  ;; Called by move when preparing to move thing into
  ;; container.  Normally, this does nothing, but
  ;; if you want to prevent the object from being moved,
  ;; you can throw an exception here.
  (define (prepare-to-move! container thing)
    (begin (when (string-suffix? (description container) "door")
             (error "You can't move doors."))
           (when (string-suffix? (description container) "closet")
             (error "You can't move your closet."))
           (when (string-suffix? (description container) "crowd")
             (error "You can't move the crowd."))
           (when (string-suffix? (description container) "well")
             (error "You can't move the well."))
           (when (string-suffix? (description container) "shack")
             (error "You can't move the shack."))
           (when (npc? container)
             (error "You can't move people!"))
           (when (string-suffix? (description container) "office")
             (error "You can't move the constable's office."))
           (when (string-suffix? (description container) "house")
             (error "You can't move your house."))
           (void)))
  )

;; initialize-thing!: thing -> void
;; EFFECT: adds thing to its initial location
(define (initialize-thing! thing)
  (add! (thing-location thing)
        thing))

;; new-thing: string container -> thing
;; Makes a new thing with the specified adjectives, in the specified location,
;; and initializes it.
(define (new-thing adjectives yours? the? specific? examine-text location take-text drop-text)
  (local [(define thing (make-thing (string->words adjectives)
                                    yours? the? specific? examine-text '() location take-text drop-text))]
    (begin (initialize-thing! thing)
           thing)))

;;;
;;; DOOR
;;; A portal from one room to another
;;; To join two rooms, you need two door objects, one in each room
;;;

(define-struct (door thing)
  ;; destination: container
  ;; The place this door leads to
  (destination)
  
  #:methods
  ;; go: door -> void
  ;; EFFECT: Moves the player to the door's location and (look)s around.
  (define (go door)
    (begin (move! me (door-destination door))
           (golook)
           (void)))
  )

;; (back): void
;; EFFECT: Makes the player move back to the previous room if it is too dark
(define (back)
  (begin
  (if (room-dark? (here))
      (go (the exit door))
      (error "You don't need this command if you can see!"))
  (void)))

;; join: room string room string
;; EFFECT: makes a pair of doors with the specified adjectives
;; connecting the specified rooms.
(define (join! room1 adjectives1 yours1 the1 room2 adjectives2 yours2 the2)
  (local [(define r1->r2 (make-door (string->words adjectives1) yours1 the1 #f ""
                                    '() room1 "" "" room2))
          (define r2->r1 (make-door (string->words adjectives2) yours2 the2 #f ""
                                    '() room2 "" "" room1))]
    (begin (initialize-thing! r1->r2)
           (initialize-thing! r2->r1)
           (void))))

;;;
;;; PERSON
;;; A character in the game.  The player character is a person.
;;;

(define-struct (person thing)
  ())

;; initialize-person: person -> void
;; EFFECT: do whatever initializations are necessary for persons.
(define (initialize-person! p)
  (initialize-thing! p))

;; new-person: string container -> person
;; Makes a new person object and initializes it.
(define (new-person adjectives yours? the? specific? examine-text location)
  (local [(define person
            (make-person (string->words adjectives)
                         yours?
                         the? 
                         specific?
                         examine-text
                         '()
                         location
                         "" ""
                         ))]
    (begin (initialize-person! person)
           person)))

;; This is the global variable that holds the person object representing
;; the player.  This gets reset by (start-game)
(define me empty)

;;;
;;; PROP
;;; A thing in the game that doesn't serve any purpose other than to be there.
;;;

(define-struct (prop thing)
  (;; noun-to-print: string
   ;; The user can set the noun to print in the description so it doesn't just say "prop"
   noun-to-print
   )
  
  #:methods
  (define (noun prop)
    (prop-noun-to-print prop)))



;; new-prop: string container -> prop
;; Makes a new prop with the specified description.
(define (new-prop description yours? the? examine-text location take-text drop-text)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define prop (make-prop adjectives yours? the? #t examine-text '() location take-text drop-text noun))]
    (begin (initialize-thing! prop)
           prop)))

;;;
;;; ADD YOUR TYPES HERE!
;;;


;;;
;;; NPC
;;; An npc that you can talk to. Due to the recommendation that
;;; we use very limited character ability in this game, each
;;; NPC has 5 dialogue options that you go through
;;;

(define-struct (npc prop)
  ;; talking?: boolean
  ;; Whether or not you are talking to this NPC
  (talking?

   ;; current: number
   ;; The current dialogue option the NPC is on
   current
   ;; d1 through d5: string
   ;; The NPC's dialogue options that it goes through
   d1 d2 d3 d4 d5)

  #:methods

  ;; talk: npc -> void
  ;; the way you intiate and partake in dialogue with an NPC
  (define (talk npc)
    (begin (when (not (npc-talking? npc))
             (textbox (string-append "You begin talking to " (string-append (descriptionthe npc) "."))))
           (set-npc-talking?! npc #t)    
           (newline)
           (newline)
           (case (npc-current npc)
             [(0) (begin (dialoguebox npc (npc-d1 npc))
                         (set-npc-current! npc 1)
                         (newline)
                         (newline)
                         (void))]
             [(1) (begin (dialoguebox npc (npc-d2 npc))
                         (set-npc-current! npc 2)
                         (newline)
                         (newline)
                         (void))]
             [(2) (begin (dialoguebox npc (npc-d3 npc))
                         (set-npc-current! npc 3)
                         (newline)
                         (newline)
                         (void))]
             [(3) (begin (dialoguebox npc (npc-d4 npc))
                         (set-npc-current! npc 4)
                         (newline)
                         (newline)
                         (void))]
             [(4) (begin (dialoguebox npc (npc-d5 npc))
                         (set-npc-current! npc 5)
                         (newline)
                         (newline)
                         (void))]
             [(5) (begin (dialoguebox npc (npc-d5 npc))
                         (newline)
                         (newline)
                         (void))])))
  
  (define (stop-talking npc)
    (if (npc-talking? npc)
        (begin (textbox(string-append "You stop talking to " (string-append (description npc) ".")))
               (set-npc-talking?! npc #f)
               (when (= (npc-current npc) 5)
                 (set-npc-current! npc 0))
               (void))
        (error "You weren't talking to them in the first place.")))
  )

(define (new-npc description yours? the? examine-text location d1 d2 d3 d4 d5)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define the-npc (make-npc adjectives yours? the? #t examine-text '() location "" "" noun #f 0 d1 d2 d3 d4 d5))]
    (begin (initialize-thing! the-npc)
           the-npc)))

;;;
;;; BED
;;; A bed that you can sleep in
;;;

(define-struct (bed thing)

  ;; sleep-text 1, 2, 3 : String
  ;; sleeping on beds displays three texts in succession.
  (sleep-text-1 sleep-text-2 sleep-text-3)
  
  #:methods

  ;; sleepin : bed -> void
  ;; Sleeps in a bed
  (define (sleepin b)
    (begin
      (textbox (bed-sleep-text-1 b))
      (newline)
      (newline)
      (sleep 2)
      (display-line "      zzz       zzz       zzz")
      (newline)
      (sleep 2)
      (textbox (bed-sleep-text-2 b))
      (newline)
      (newline)
      (sleep 2)
      (display-line "     zz      zz      zz      zz      zz")
      (newline)
      (sleep 2)
      (textbox (bed-sleep-text-3 b))
      (newline)
      (void)))
    )

(define (new-bed adjectives yours? the? specific? examine-text location sleep-text-1 sleep-text-2 sleep-text-3)
  (local [(define the-bed (make-bed (string->words adjectives)
                                      yours?
                                      the?
                                      specific?
                                      examine-text
                                    '() location "" "" sleep-text-1 sleep-text-2 sleep-text-3))]
    (begin (initialize-thing! the-bed)
           the-bed)))


;;;
;;; BOX
;;; A box that has stuff in it
;;;

(define-struct (box thing)
  ;; noun-to-print: string
  ;; The user can set the noun to print in the description so it doesn't just say "box"
  (noun-to-print)
  
  #:methods
  (define (noun b)
    (box-noun-to-print b)))



(define (new-box description yours? the? specific? examine-text contents location take-text drop-text)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define the-box (make-box adjectives
                                      yours?
                                      the?
                                      specific?
                                      examine-text
                                      contents
                                      location
                                      take-text
                                      drop-text
                                      noun))]
    (begin (initialize-thing! the-box)
           the-box)))

;;;
;;; HAY
;;; Hay that you can sift through to reveal hidden treasures
;;;

(define-struct (hay thing)
  ;; treasure: listof procedure
  ;; the treasure to be generated when you sift through this hay.
  (treasure)
  #:methods

  ;; sift: hay -> void
  ;; sifts through the hay to reveal the treasures
  (define (sift h)
    (begin (for-each (λ (t) (begin (initialize-thing! t) t)) (hay-treasure h))
           (textboxonlist (append (list "You sift through the hay. Within it you find:" "") (map (λ (element) (string-append "- " (string-append (capital-description element) " -"))) (hay-treasure h)) (list "" "Those items are now in your inventory.")))
           (destroy! h)
           (void)))
  )

(define (new-hay description yours? the? specific? examine-text location take-text drop-text treasure)
  (local [(define the-hay (make-hay (string->words description)
                                    yours?
                                    the?
                                    specific?
                                    examine-text
                                    '()
                                    location
                                    take-text
                                    drop-text
                                    treasure))]
    (begin (initialize-thing! the-hay)
           the-hay)))


;;;
;;; CHEST
;;; A chest that requires a specific key to open
;;;


(define-struct (chest thing)

  ;; chestid: number
  ;; Keys with this same id can open the chest
  (chestid


   ;; open?: boolean
   ;; Whether the chest is open or not
   open?

   ;; opened-examine-text: string
   ;; The new examine-text for this chest when it is open
   opened-examine-text

   ;; closed-examine-text: string
   ;; The new new examine-text for this chest when it is closed. Feel free to set to the initial examine-text.
   closed-examine-text
   )

  #:methods

  ;; open: chest key -> void
  ;; Opens the chest if the key is correct. Otherwise, throws an exception.
  (define (open c k)
    (if (not (chest-open? c))
        (if (= (chest-chestid c) (key-keyid k))
            (begin (textboxonlist (list (string-append (string-append (string-append (string-append "You open "
                                                                                                    (descriptionthe c))
                                                                                     " using ")
                                                                      (descriptionthe k))
                                                       ".")
                                        (chest-opened-examine-text c)))
                   (set-chest-open?! c #t)
                   (set-object-examine-text! c (chest-opened-examine-text c)))
            (error "This isn't the correct key."))
        (error "It's already open.")))


  ;; close: chest key -> void
  ;; Closes the chest if the key is correct. Otherwise, throws an exception.
  (define (close c k)
    (if (chest-open? c)
        (if (= (chest-chestid c) (key-keyid k))
            (begin (textbox (string-append (string-append (string-append (string-append "You close "
                                                                                        (descriptionthe c))
                                                                         " using ")
                                                          (descriptionthe k))
                                           "."))
                   (set-chest-open?! c #f)
                   (set-object-examine-text! c (chest-closed-examine-text c)))
            (error "You need the correct key."))
        (error "It's already closed.")))

  )

(define (new-chest description yours? the? specific? examine-text contents location take-text drop-text chestid openorclosed open-examine-text closed-examine-text)
  (local [(define the-chest
            (make-chest (string->words description)
                                        yours?
                                        the?
                                        specific?
                                        examine-text
                                        contents
                                        location
                                        take-text
                                        drop-text
                                        chestid
                                        openorclosed
                                        open-examine-text
                                        closed-examine-text))]
    (begin (initialize-thing! the-chest)
           the-chest)))


;;;
;;; KEY
;;; A key that opens a specific chest
;;;

(define-struct (key thing)

  ;; keyid: number
  ;; Chests with this same id can be opened by this key
  (keyid))


(define (new-key description yours? the? specific? examine-text location take-text drop-text keyid)
  (local [(define the-key (make-key (string->words description)
                                    yours?
                                    the?
                                    specific?
                                    examine-text
                                    '()
                                    location
                                    take-text
                                    drop-text
                                    keyid))]
    (begin (initialize-thing! the-key)
           the-key)))

;;;
;;; FLASHLIGHT
;;; A flashlight that allows you to see in dark rooms
;;;

(define-struct (flashlight thing)
  ;; on?: boolean
  ;; Whether or not the flashlight is on
  (on?)
  
  #:methods
  ;; switch: flashlight -> void
  ;; Flips the flashlight from off to on or from on to off
  (define (switch f)
    (begin
    (if (not (flashlight-on? f))
      (begin
      (set-flashlight-on?! f #t)
      (if (room-dark? (here))
          (textboxonlist (cons "You turn on the flashlight." (cons "" (cons "You see:" (fancy-describe-contents (here))))))
          (textbox "You turn on the flashlight.")))
      (begin
      (set-flashlight-on?! f #f)
      (textbox "You turn off the flashlight.")))
    (void)))

  )

(define (new-flashlight description yours? the? specific? examine-text location take-text drop-text on?)
  (local [(define the-flashlight (make-flashlight (string->words description)
                                    yours?
                                    the?
                                    specific?
                                    examine-text
                                    '()
                                    location
                                    take-text
                                    drop-text
                                    on?))]
    (begin (initialize-thing! the-flashlight)
           the-flashlight)))


;;;
;;; USER COMMANDS
;;;

(define (golook)
  (begin (if (canyousee? (here))
             (textboxonlist (cons (golookdesc (here)) (cons (room-go-text (here)) (cons "" (cons "You see:" (fancy-describe-contents (here)))))))
             (textboxonlist (list (golookdesc (here)) "It's too dark to see anything. If only you had some source of light." "Type (back) to go back.")))
         (void)))

(define (look)
  (begin (if (canyousee? (here))
             (textboxonlist (cons (lookdesc (here)) (cons (room-go-text (here)) (cons "" (cons "You see:" (fancy-describe-contents (here)))))))
             (textboxonlist (list (lookdesc (here)) "It's too dark to see anything. If only you had some source of light." "Type (back) to go back.")))
         (void)))

(define-user-command (look) "Prints what you can see in the room.")

(define (inventory)
  (if (empty? (my-inventory))
      (textbox "You don't have anything.")
      (textboxonlist (cons "You have:" (map (λ (o) (string-append o " -")) (map (λ (o) (string-append "- " o)) (map (λ (o) (capital-description o)) (my-inventory))))))))

(define-user-command (inventory)
  "Prints the things you are carrying with you.")

(define-user-command (examine (the THING))
  "Takes a closer look at the thing.")

(define (take thing)
  (if (have? thing)
      (textbox "You already have it.")       
      (begin(move! thing me)
            (textboxonlist
             (if (string=? (thing-take-text thing) "")           
                 (list (string-append "You take " (string-append (descriptionthe thing) ".")))
                 (append (list (string-append "You take " (string-append (descriptionthe thing) ".")))
                         (substrings (thing-take-text thing))))))))

(define-user-command (take (the THING))
  "Moves a thing to your inventory.
Only works with objects that are not in containers.")

(define-user-command (within (the CONTAINER) THING)
  "Same as from, just another way of typing it.
Example use: (examine (within (the crowd) corpse))")

(define-user-command (from (the CONTAINER) THING)
   "The way to access something that is inside a container.
IMPORTANT: Do not type (the THING) and rather just type THING.
Example use: (take (from (the coatrack) coat))")

(define (drop thing)
  (if (not (have? thing))
      (textbox "You don't have it.")
      (begin(move! thing (here))
            (textboxonlist
             (if (string=? (thing-drop-text thing) "")           
                 (list (string-append "You drop " (string-append (descriptionthe thing) ".")))
                 (append (list (string-append "You drop " (string-append (descriptionthe thing) ".")))
                         (substrings (thing-drop-text thing))))))))

(define-user-command (drop (the THING))
  "Removes a thing from your inventory and places it in the room.")

(define (put thing container)
  (local [(define (inoron inp)
              (if (or (string-suffix? (description inp) "coatrack")
                      (string-suffix? (description inp) "bookshelf")
                      (string-suffix? (description inp) "desk")
                      (string-suffix? (description inp) "bed")
                      (string-suffix? (description inp) "table"))
                  " on "
                  " in "))]
  (begin(move! thing container)
        (textboxonlist
         (if (string=? (thing-drop-text thing) "")
             (list (string-append (string-append (string-append (string-append "You put " (descriptionthe thing)) (inoron container)) (descriptionthe container))"."))
             (list (string-append (string-append (string-append (string-append "You put " (descriptionthe thing)) (inoron container)) (descriptionthe container))".")
                   (thing-drop-text thing)))))))

(define-user-command (put (the THING) (the CONTAINER))
  "Moves the thing from its current location
and puts it in the container.")

(define (indivlist inp)
  (string-append (string-append (string-append (string-append "- "(string-append (~a (first inp)) " -")) (string-append (string #\newline) (second inp))) (string #\newline))(string #\newline)))

(define (helplist inp)
  (if (= (length inp) 0)
      "*** You can use 'your' rather than 'the' for objects that are yours ***"
      (string-append (indivlist (first inp)) (helplist (rest inp)))))

(define (help)
 (textbox (string-append (string-append (string-append "ALL POSSIBLE COMMANDS:" (string #\newline)) (string #\newline)) (helplist (all-user-commands)))))

(define-user-command (help)
  "Displays this help information.")

(define-user-command (go (the DOOR))
  "Go through the door to its destination.")

(define (check condition)
  (if condition
      (display-line "Check succeeded")
      (error "Check failed!!!")))

(define-user-command (check CONDITION)
  "Throws an exception if condition is false.")

;;;
;;; ADD YOUR COMMANDS HERE!
;;;

(define-user-command (sleepin (the BED))
  "Sleep in a bed, if you can.")

(define-user-command (contents (the THING))
  "Lists the contents of the thing, if any.")

(define-user-command (open (the CHEST) (the KEY))
  "Opens the chest using the ke.y")

(define-user-command (close (the CHEST) (the KEY))
  "Closes the chest using the key.")

(define-user-command (switch (the FLASHLIGHT))
  "Turns a flashlight from off to on, or from on to off.")

(define-user-command (sift (the HAY))
  "Sifts through hay to reveal any hidden treasures within.")

(define-user-command (talk (the CHARACTER))
  "Talks to a character. Repeat this command to continue talking.")

(define-user-command (stop-talking (the CHARACTER))
  "Stops talking to a character.")

(define-user-command (back)
  "Goes back to the previous room if it's too dark to see.")


;;;
;;; THE GAME WORLD - FILL ME IN
;;;


;; titlescreenimage
;; image to be used in the title screen
(define titlescreenimage
"▓▓▓▓▓▓▓███████▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓████████████▓▓▓▓▓▓▓▓▓▓|------------------|▓▓▓▓▓
▓▓▓▓▓███████████▓▓▓▓▓▓▓▓▓▓▓|  Adventure Game  |▓▓▓▓▓
▓▓▓███████████████▓▓▓▓▓▓▓▓▓| By Royce Mettler |▓▓▓▓▓
▓▓▓▓████████████▓▓▓▓▓▓▓▓▓▓▓| CS 111 Fall 2021 |▓▓▓▓▓
▓▓▓▓▓▓█████████▓▓▓▓▓▓▓▓▓▓▓▓|------------------|▓▓▓▓▓
▓▓▓▓▓▓▓▓▓███▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▓▓▓▒▒▒▒▒▒▒
▒▒▒▒▒▒▓▓▓▓▓▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
▒▒▒▒▒▓▓▓▓▓▓▓▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
▒▒▒▒▓▓▓▓▓▓▓▓▓▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░    ░░░░░░░░       
▒▒▒▓▓▓▓▓▓▓▓▓▓▓▒▒▒▒▒░░                               
▒▒▓▓▓▓▓▓▓▓▓▓░░░░       Please use horizontal mode.  
▒▓▓▓▓░░░          Type (help) to list all commands. 
▓░░░       I hope you enjoy! Type (start) to begin! "
)

;; titlescreen -> void
;; Displays the title screen. Called upon running the program.

(define (titlescreen)
  (imgbox titlescreenimage))

;; startuptext: -> list
;; The initial text to display upon typing (start), in list form for the purpose of textboxing
(define (startuptext)
  (list "You wake up. It's 8:02 AM." "Nothing like a good night's sleep, especially these days."))

;; start -> void
;; User-friendly way of running (start-game) followed by a modified version of (look)
;; to include the startup text.
(define (start)
  (begin (start-game)
         (textboxonlist (append (startuptext) (cons (lookdesc (here))
                                                    (cons "" (cons "You see:" (fancy-describe-contents (here)))))
                                (list "" "Suggested commands:"
                                      "(examine (the THING)), (take (the THING)),"
                                      "(contents (the THING)), (help)"
                                      "THING = name of object, i.e. 'bookshelf', 'hall door'" )))
         (void)))

;; start-game: -> void
;; Recreate the player object and all the rooms and things.
(define (start-game)
  ;; Fill this in with the rooms you want
  (local [(define starting-room (new-room "bedroom" #t #f "It's just as you left it." #f))
          (define living-room (new-room "living room" #t #f "Nobody else seems to be home." #f))
          (define town-square (new-room "town square" #f #t "Everyone is crowded around something. You can't make out what it is." #f))
          (define shack (new-room "decrepit shack" #f #f "It's pretty disgusting in there." #f))
          (define office (new-room "constable's office" #f #t "The constable is there. He looks troubled." #f))
          (define well (new-room "bottom of the well" #f #t "It's a rather expansive cavern. You're surprised." #t))
          (define deepcavern (new-room "deep cavern" #f #t "Evidently, this was dug out by someone. Why?" #t))]
    (begin
      (set! me (new-person "" #f #f #t "" starting-room))

      ;; YOUR BEDROOM : Joined with the living room
      (new-chest "old toy"
                 #t
                 #f
                 #t
                 "It's been locked for years. Maybe if you found the key you could open it."
                 '()
                 starting-room
                 "It's light. It feels like nothing is inside."
                 ""
                 1
                 #f
                 "There really wasn't anything inside. What a shame."
                 "There wasn't anything inside earlier.\nThere's not gonna be something now.")
      (new-bed ""
               #t
               #f
               #t
               "You haven't been able to sleep recently. Nobody in town has."
               starting-room
               "You lie down in your bed and close your eyes."
               "It's been so stressful lately. It's been so hard to sleep."
               "Unfortunately, you weren't able to sleep at all since you just woke up.")
      (new-prop "desk"
                #t
                #f
                "You've spent countless hours here, studying god knows what."
                starting-room
                ""
                "")
      (new-prop "coat"
                #t
                #f
                "You always thought its pattern, brown with white polka dots,\nwas ugly, but it was a gift from your grandmother so you kept quiet."
                (new-box "coatrack"
                         #t
                         #f
                         #t
                         "Your old coat is hanging on it."
                         '()
                         starting-room
                         "It's pretty heavy, but maybe it'll be useful."
                         "You probably wouldn't have needed it anyway.")
                "It doesn't look *that* bad on you."
                "No actually it did look that bad on you.")
      (new-prop "letter-opener"
                #t
                #f
                "It bears the insignia of your dad's papercutting business." 
                (new-box "bookshelf"
                         #t
                         #f
                         #t
                         "There haven't been any books on it for years.\nThere is a letter-opener on it, though."
                         '()
                         starting-room
                         "You have to spend a couple minutes breaking it down\ninto wooden planks first, but hopefully it was worth it."
                         "You don't feel like reassembling it,\nso you just throw the planks angrily on the floor.")
                "It's kind of sharp."
                "")
      (join! starting-room "hall" #t #f
             living-room "bedroom" #t #f)


      ;; YOUR LIVING ROOM : Joined with the bedroom and the town square

      (local [(define closet (new-box "closet"
                                      #t
                                      #f
                                      #t
                                      "You notice your father's coat is missing.\nYou also notice a small key on the closet floor."
                                      '()
                                      living-room
                                      ""
                                      ""))]
        (begin (new-key "small white"
                        #f
                        #f
                        #t
                        "What could it unlock?"
                        closet
                        ""
                        ""
                        1)
               (new-prop "random assortment of clothes"
                #f
                #f
                "There's all sorts of stuff in here.\nYou don't feel like going through it."
                closet
                "It almost engulfs you."
                "You are glad to stop having to carry all of it.")))
      (new-box "dinner table"
               #t
               #f
               #t
               "Your mother's meals really were the best."
               '()
               living-room
               ""
               "")
      (join! living-room "front" #t #f
             town-square "front" #t #f)

      ;; THE TOWN SQUARE : Joined with the living room, the shack, and the well

      (local [(define largecrowd (new-box "large crowd"
                                          #f
                                          #f
                                          #t
                                          "As you make your way through the crowd,\nyou realize they are all crowded around a corpse."
                                          '()
                                          town-square
                                          ""
                                          ""))]
        (begin (new-prop "corpse"
                         #f
                         #f
                         "Female. Maybe in her 30s. She is very pale.\nShe's covered in handprints of all sizes.\nShe looks... happy. Almost relieved."
                         largecrowd
                         "Several members of the crowd cry out in shock.\nOne man angrily tells you to put her back down."
                         "Most of the crowd is still staring confusedly at you.\nA young girl begins to cry.")))
      (new-npc "father"
               #t
               #f
               "He looks incredibly disturbed."
               town-square
               "Oh hey, you're awake. Good to see you.\nHope you slept well."
               "I woke up this morning to the sounds of\ncommotion outside, and I went to check."
               "It looks like someone was murdered, or something.\nI can't really get a good look at the body through the crowd."
               "I'm not sure what's going on, but if it's\nin any way connected to the recent events, we should keep out of it."
               "We best keep our doors and windows\nlocked from now on. I love you.")

      (new-prop "old well"
                #f
                #f
                "You've never examined the well before.\nIt's always been there, though."
                town-square
                ""
                "")

      (new-prop "old decrepit shack"
                #f
                #f
                "You see this every day. Nobody in town\nknows who it once belonged to."
                town-square
                ""
                "")

      (new-prop "constable's office"
                #f
                #t
                "It's a pretty small building.\nIt has evidence of a faded logo on the front,\nbut you can't tell what that logo once looked like."
                town-square
                ""
                "")

      (new-prop "house"
                #t
                #f
                "It looks the same as it did 18 years ago.\nNot that you remember what it looked like when you were born."
                town-square
                ""
                "")

      (join! town-square "shack" #f #t
             shack "front" #f #t)

      (join! town-square "constable's office" #f #t
             office "front" #f #t)

      (join! town-square "well ladder" #f #t
             well "well exit ladder" #f #t)

      ;; DECREPIT SHACK : Joined with the town square

      (new-hay "pile of"
               #f
               #f
               #t
               "Maybe if you (sift through it, you might find something cool."
               shack
               "You can clearly feel stuff inside it.\nMaybe you should (sift through it."
               "At this point you can see stuff poking out of it."
               (list (make-key (string->words "old wooden")
                               #f
                               #f
                               #t
                               "What could it open?\nYou look over at the chest. It really is a mystery."
                               '()
                               me
                               ""
                               ""
                               2)
                     (make-prop (string->words "shiny")
                                #f
                                #f
                                #t
                                "It glistens in the sunlight. These are pretty valuable."
                                '()
                                me
                                "You hastily shove it into your inventory.\nSilver-dollars are hard to come by these days."
                                "Maybe it was too good to be true?\nProbably not, though. You should probably pick it back up."
                                "silver-dollar")
                     ))
      (new-flashlight ""
                      #f
                      #f
                      #t
                      "It's surprisingly bright."
                      (new-chest "moldy old"
                                 #f
                                 #f
                                 #t
                                 "It looks like it's been here since the middle ages."
                                 '()
                                 shack
                                 "There's something inside, rolling around."
                                 "You can sense that no matter how hard you try\nto break it open, you wouldn't be able to."
                                 2
                                 #f
                                 "Inside is a flashlight."
                                 "You're pretty surprised you found a\nfully functional flashlight in that thing.")
                      "It'll certainly be useful."
                      "That was probably a bad idea."
                      #f)
      (new-bed "disgusting slimy filthy old"
               #f
               #f
               #t
               "It hasn't been slept on in years, although\nthere is still a human-sized imprint in the filthy mattress."
               shack
               "You attempt to go to sleep on this thing even though you pretty much just woke up."
               "You begin to contemplate life."
               "You were officially not able to sleep. It is now 8:14 AM.")

      ;; THE CONSTABLE'S OFFICE: joined with the town square
      (new-prop "town constable's desk"
                #f
                #t
                "It's pretty cluttered."
                office
                ""
                "")
       (new-npc "town constable"
                #f
                #t
                "He has a grave look striken across his face."
                office
                "You're that papercutter's kid,\naren't you. I hope the hubbub didn't wake you up."
                "People have been coming in all day,\nasking what happened. To be honest I don't know."
                "Everyone's asking about the handprints,\nbut nobody's asking about the trail of handprints leading to the well."
                "Not like I have any information on that\ntrail either. Most probably didn't even notice it."
                "All I'll say is, don't follow that trail.\nYou don't wanna end up like her. Trust me.")

       ;; THE WELL: joined with the town square and the deep cavern
       (new-hay "damp pile of"
               #f
               #f
               #t
               "It's pretty much just mud and only a bit of hay. It's disgusting."
               well
               "Ew."
               "Not sure why you picked it up in the first place."
               (list (make-key (string->words "tiny iron")
                               #f
                               #f
                               #t
                               "You almost missed it in the hay. It's miniscule."
                               '()
                               me
                               ""
                               ""
                               3)
                     ))
       (new-prop "severely damaged lighter"
                 #f
                 #f
                 "It looks like it was dropped by someone in a rush."
                 well
                 "It almost falls apart in your hand."
                 "")
       (new-prop "pile of coins"
                 #f
                 #f
                 "So many wishes. You wonder how many came true."
                 well
                 "It feels immoral."
                 "You think it'll be better that way.")
       (join! well "gigantic wooden" #f #f
              deepcavern "gigantic wooden exit" #f #t)

       ;; THE DEEP CAVERN: joined with the well
       (new-npc "young man"
                #f
                #f
                "He looks not even 20. He is wearing a purple shirt with an N."
                deepcavern
                "Hello. I've been waiting for you."
                "I hope you've been having fun. I have."
                "Unfortunately, it is at this point that I\nmust attend to my other tasks. Namely, my essay for my seminar."
                "I've had a lot of fun programming this game.\nMaybe I'll come back to it eventually. Probably."
                "But for now, this is goodbye.\nDream big, dream  safe, and goodnight.")

       (new-prop "trophy"
                 #t
                 #f
                 "Thanks for playing!"
                 (new-chest "large iron"
                            #f
                            #f
                            #t
                            "The endgame."
                            '()
                            deepcavern
                            "There's something inside, rolling around."
                            "You can sense that no matter how hard you try\nto break it open, you wouldn't be able to."
                            3
                            #f
                            "Inside is a trophy."
                            "")
                 "You deserve it."
                 ":(")
       
      ;; UTILITIES
      (check-containers!)
      (void))))

;;;
;;; PUT YOUR WALKTHROUGHS HERE
;;;

(define-walkthrough gotoconstable
  (start)
  (go (the door))
  (go (the front door))
  (go (the office))
  )

(define-walkthrough gotohay
  (start)
  (go (the door))
  (go (the front door))
  (go (the shack)))

(define-walkthrough win
  (start)
  (go (the door))
  (go (the front door))
  (go (the shack door))
  (sift (the hay))
  (open (the chest) (the key))
  (take (within (the chest) flashlight))
  (switch (the flashlight))
  (go (the door))
  (go (the well ladder door))
  (sift (the hay))
  (go (the gigantic wooden door))
  (open (the chest) (the tiny iron key))
  (take (within (the chest) trophy))
  (examine (the trophy)))

;;;
;;; UTILITIES
;;;

;; here: -> container
;; The current room the player is in
(define (here)
  (thing-location me))

;; stuff-here: -> (listof thing)
;; All the stuff in the room the player is in
(define (stuff-here)
  (container-accessible-contents (here)))

;; stuff-here-except-me: -> (listof thing)
;; All the stuff in the room the player is in except the player.
(define (stuff-here-except-me)
  (remove me (stuff-here)))

;; my-inventory: -> (listof thing)
;; List of things in the player's pockets.
(define (my-inventory)
  (container-accessible-contents me))

;; accessible-objects -> (listof thing)
;; All the objects that should be searched by find and the.
(define (accessible-objects)
  (append (stuff-here-except-me)
          (my-inventory)))

;; have?: thing -> boolean
;; True if the thing is in the player's pocket.
(define (have? thing)
  (eq? (thing-location thing)
       me))

;; have-a?: predicate -> boolean
;; True if the player as something satisfying predicate in their pocket.
(define (have-a? predicate)
  (ormap predicate
         (container-accessible-contents me)))

;; find-the: (listof string) -> object
;; Returns the object from (accessible-objects)
;; whose name contains the specified words.
(define (find-the words)
  (find (λ (o)
          (andmap (λ (name) (is-a? o name))
                  words))
        (accessible-objects)))

;; find-within: container (listof string) -> object
;; Like find-the, but searches the contents of the container
;; whose name contains the specified words.
(define (find-within container words)
  (find (λ (o)
          (andmap (λ (name) (is-a? o name))
                  words))
        (container-accessible-contents container)))

;; find: (object->boolean) (listof thing) -> object
;; Search list for an object matching predicate.
(define (find predicate? list)
  (local [(define matches
            (filter predicate? list))]
    (case (length matches)
      [(0) (error "There's nothing like that here")]
      [(1) (first matches)]
      [else (error "Which one?")])))

;; everything: -> (listof container)
;; Returns all the objects reachable from the player in the game
;; world.  So if you create an object that's in a room the player
;; has no door to, it won't appear in this list.
(define (everything)
  (local [(define all-containers '())
          ; Add container, and then recursively add its contents
          ; and location and/or destination, as appropriate.
          (define (walk container)
            ; Ignore the container if its already in our list
            (unless (member container all-containers)
              (begin (set! all-containers
                           (cons container all-containers))
                     ; Add its contents
                     (for-each walk (container-contents container))
                     ; If it's a door, include its destination
                     (when (door? container)
                       (walk (door-destination container)))
                     ; If  it's a thing, include its location.
                     (when (thing? container)
                       (walk (thing-location container))))))]
    ; Start the recursion with the player
    (begin (walk me)
           all-containers)))

;; print-everything: -> void
;; Prints all the objects in the game.
(define (print-everything)
  (begin (display-line "All objects in the game:")
         (for-each print-description (everything))))

;; every: (container -> boolean) -> (listof container)
;; A list of all the objects from (everything) that satisfy
;; the predicate.
(define (every predicate?)
  (filter predicate? (everything)))

;; print-every: (container -> boolean) -> void
;; Prints all the objects satisfying predicate.
(define (print-every predicate?)
  (for-each print-description (every predicate?)))

;; check-containers: -> void
;; Throw an exception if there is an thing whose location and
;; container disagree with one another.
(define (check-containers!)
  (for-each (λ (container)
              (for-each (λ (thing)
                          (unless (eq? (thing-location thing)
                                       container)
                            (error (description container)
                                   " has "
                                   (description thing)
                                   " in its contents list but "
                                   (description thing)
                                   " has a different location.")))
                        (container-contents container)))
            (everything)))

;; is-a?: object word -> boolean
;; True if word appears in the description of the object
;; or is the name of one of its types
(define (is-a? obj word)
  (let* ((str (if (symbol? word)
                  (symbol->string word)
                  word))
         (probe (name->type-predicate str)))
    (if (eq? probe #f)
        (member str (description-word-list obj))
        (or(probe obj)
           (member str (description-word-list obj))))))

;; display-line: object -> void
;; EFFECT: prints object using display, and then starts a new line.
(define (display-line what)
  (begin (display what)
         (newline)
         (void)))

;; words->string: (listof string) -> string
;; Converts a list of one-word strings into a single string,
;; e.g. '("a" "red" "door") -> "a red door"
(define (words->string word-list)
  (string-append (first word-list)
                 (apply string-append
                        (map (λ (word)
                               (string-append " " word))
                             (rest word-list)))))

;; string->words: string -> (listof string)
;; Converts a string containing words to a list of the individual
;; words.  Inverse of words->string.
(define (string->words string)
  (string-split string))

;; add-a-or-an: (listof string) -> (listof string)
;; Prefixes a list of words with "a" or "an", depending
;; on whether the first word in the list begins with a
;; vowel.
(define (add-a-or-an word-list)
  (local [(define first-word (first word-list))
          (define first-char (substring first-word 0 1))
          (define starts-with-vowel? (string-contains? "aeiou" first-char))]
    (cons (if starts-with-vowel?
              "an"
              "a")
          word-list)))

;; add-your: (listof string) -> (listof string)
;; Prefixes a word with "your" if it describes a game
;; object that belongs to you
(define (add-your word-list)
  (cons "your" word-list))

;; add-the (listof string) -> (listof string)
;; Prefixes a word with "the" if it describes something
;; that should start with the i.e. The constable
(define (add-the word-list)
  (cons "the" word-list))


;;;
;;; TEXTBOXES
;;;

;; makesubstring: string -> string
;; Takes a string of multiple lines and makes a new
;; string out of only the last line

(define (makesubstring str)
  (local [
(define (makesubstringhelper str1 str2)
  (if (= (string-length str1) 0)
      str2
  (if (char=? (string-ref str1 0) #\newline)
      (substring str1 1)
      (makesubstringhelper (substring str1 1) str2))))]
    (makesubstringhelper str str)))

;; substrings: string -> (listof string)
;; Makes a list of all lines of a string of multiple lines

(define (substrings str)
  (if (string=? str (makesubstring str))
      (list str)
      (cons
       (substring str 0 (- (- (string-length str) (string-length(makesubstring str)))1))
       (substrings (makesubstring str)))))

;; longest: (listof string) -> string
;; Returns the longest string in a list of strings

(define (longest inputlist)
  (if (= (length inputlist) 1)
      (first inputlist)
  (if (>= (string-length (first inputlist)) (string-length (first (rest inputlist))))
      (longest (cons (first inputlist) (rest (rest inputlist))))
      (longest (rest inputlist)))))

;; halfdifference: number number -> number
;; Returns half of the difference between two numbers, rounding
;; down if this number ends in .5. Used in the process of centering
;; the lines within a text box.

(define (halfdifference input1 input2)
  (if (= (modulo (- input2 input1) 2) 1)
      (/ (- (- input2 1) input1) 2)
      (/ (- input2 input1) 2)))

;; centerone: string string -> string
;; Takes a small string and a big string and makes the small string
;; the same length as the big string by adding space characters onto
;; each side of the text.

(define (centerone smallelement bigelement)
  (string-append
   (string-append
    (make-string (halfdifference (string-length smallelement) (string-length bigelement)) #\ )
    smallelement)
   (make-string (halfdifference (string-length smallelement) (string-length bigelement)) #\ )))

;; textbox: string -> void
;; Prints a textbox around a string and centers the lines of the string
;; within this printed box

(define (textbox inp)
    (begin
      (printf (string-append (string-append "{}" (make-string (+ 4 (string-length (longest (substrings inp)))) #\-)) "{}"))
      (printf "~%")
      (for-each (λ (x)
                  (begin
                    (printf " |  ")
                    (printf (centerone x (longest (substrings inp))))
                    (if (= (modulo (- (string-length (longest (substrings inp))) (string-length x)) 2) 1)
                        (printf "   | ")
                        (printf "  | "))
                    (printf "~%")
                    ))
                (substrings inp))
      (printf (string-append (string-append "{}" (make-string (+ 4 (string-length (longest (substrings inp)))) #\-)) "{}"))
      (void)
      ))

;; textboxonlist: list -> void
;; Prints a textbox around a list

(define (textboxonlist inp)
  (begin
    (printf (string-append (string-append "{}" (make-string (+ 4 (string-length (longest inp))) #\-)) "{}"))
    (printf "~%")
    (for-each (λ (x)
                  (begin
                    (printf " |  ")
                    (printf (centerone x (longest inp)))
                    (if (= (modulo (- (string-length (longest inp)) (string-length x)) 2) 1)
                        (printf "   | ")
                        (printf "  | "))
                    (printf "~%")
                    ))
                inp)
    (printf (string-append (string-append "{}" (make-string (+ 4 (string-length (longest inp))) #\-)) "{}"))
    (void)
    ))


;; imgbox: string -> void
;; Prints a box around an image

(define (imgbox inp)
    (begin
      (printf (string-append "\\\\" (string-append (make-string (string-length (first (substrings inp))) #\=) "//")))
      (printf "~%")
      (for-each (λ (x)
                  (begin
                    (printf "||")
                    (printf x)
                    (printf "||")
                    (printf "~%")
                    ))
                (substrings inp))
      (printf (string-append "//" (string-append (make-string (string-length (first (substrings inp))) #\=) "\\\\")))
      (void)
          ))

;; dialoguebox: npc string -> void
;; The box to be printed around npc dialogue
(define (dialoguebox npc inp)
    (begin
      (printf " /-----\\")
      (newline)
      (printf " |  0  |")
      (newline)
      (printf " | -|- |  ")
      (if (object-yours? npc)
          (printf (capital-description npc))
          (printf (string-append "T" (substring (descriptionthe npc) 1))))
      (printf ": ")
      (printf (first (substrings inp)))
      (newline)
      (when (= (length (substrings inp)) 2)
        (begin
        (printf " |  |  |  ")
        (printf (list-ref (substrings inp) 1))
        (newline)))
      (printf " | .^. |")
      (newline)
      (printf " \\-----/")
      (void)
          ))


;;
;; The following calls are filling in blanks in the other files.
;; This is needed because this file is in a different langauge than
;; the others.
;;
(set-find-the! find-the)
(set-find-within! find-within)
(set-restart-game! (λ () (start-game)))
(define (game-print object)
  (cond [(void? object)
         (void)]
        [(object? object)
         (print-description object)]
        [else (write object)]))

(current-print game-print)

;;;
;;; Start it up
;;;

(titlescreen)
;(start)

