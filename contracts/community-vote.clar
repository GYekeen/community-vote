;; ------------------------------------------------------------
;; community-vote.clar
;; Lightweight public community voting / polling system.
;; Anyone can vote. One vote per wallet. Multiple polls allowed.
;; ------------------------------------------------------------

(define-constant ERR-POLL-NOT-FOUND u100)
(define-constant ERR-ALREADY-VOTED u101)
(define-constant ERR-POLL-CLOSED u102)
(define-constant ERR-INVALID-OPTION u103)
(define-constant ERR-DESCRIPTION-LONG u104)

;; Poll counter
(define-data-var poll-count uint u0)

;; Poll structure:
;; id:        poll id
;; creator:   wallet that created poll
;; question:  text
;; yes:       number of yes votes
;; no:        number of no votes
;; start:     block started
;; end:       block ends
;; open:      bool - still active
(define-map polls
  { id: uint }
  { creator: principal,
    question: (string-ascii 200),
    yes: uint,
    no: uint,
    start: uint,
    end: uint,
    open: bool }
)

;; Track who voted on which poll
(define-map votes
  { id: uint, voter: principal }
  { voted: bool }
)

;; ------------------------------------------------------------
;; Create a new community poll
;; Anyone can create a poll
;; duration = number of blocks voting lasts
;; ------------------------------------------------------------

(define-public (create-poll (question (string-ascii 200)) (duration uint))
  (let ((q-len (len question))
        (id (+ (var-get poll-count) u1))
        (current-height burn-block-height)
        (checked-duration (if (> duration u0) duration u1)))
    (if (> q-len u200)
        (err ERR-DESCRIPTION-LONG)
        (begin
          (var-set poll-count id)
          (map-set polls
            { id: id }
            {
              creator: tx-sender,
              question: question,
              yes: u0,
              no: u0,
              start: current-height,
              end: (+ current-height checked-duration),
              open: true
            }
          )
          (ok id)
        )
    )
  )
)

;; ------------------------------------------------------------
;; Vote on a poll (yes / no)
;; option = true (yes), false (no)
;; ------------------------------------------------------------

(define-public (vote (poll-id uint) (option bool))
  (let ((checked-id (if (> poll-id u0) poll-id u1)))
    (match (map-get? polls { id: checked-id })
      some-poll
        (if (or (not (get open some-poll)) (> burn-block-height (get end some-poll)))
              (err ERR-POLL-CLOSED)
              (match (map-get? votes { id: checked-id, voter: tx-sender })
                some-v (err ERR-ALREADY-VOTED)
                (begin
                    ;; record vote
                    (map-set votes { id: checked-id, voter: tx-sender } { voted: true })

                    ;; update tally
                    (if option
                        (map-set polls { id: checked-id }
                          {
                            creator: (get creator some-poll),
                            question: (get question some-poll),
                            yes: (+ (get yes some-poll) u1),
                            no: (get no some-poll),
                            start: (get start some-poll),
                            end: (get end some-poll),
                            open: (get open some-poll)
                          })
                        (map-set polls { id: checked-id }
                          {
                            creator: (get creator some-poll),
                            question: (get question some-poll),
                            yes: (get yes some-poll),
                            no: (+ (get no some-poll) u1),
                            start: (get start some-poll),
                            end: (get end some-poll),
                            open: (get open some-poll)
                          })
                    )

                    (ok true)
                  )
              )
          )
      (err ERR-POLL-NOT-FOUND)
    )
  )
)

;; ------------------------------------------------------------
;; Close poll (creator only)
;; After closing, no one can vote even if time remains.
;; ------------------------------------------------------------

(define-public (close-poll (poll-id uint))
  (let ((checked-id (if (> poll-id u0) poll-id u1))
        (poll-data (map-get? polls { id: poll-id })))
    (if (is-none poll-data)
      (err ERR-POLL-NOT-FOUND)
      (let ((some-poll (unwrap-panic poll-data)))
        (if (not (is-eq (get creator some-poll) tx-sender))
          (err ERR-POLL-CLOSED)
          (begin
            (map-set polls { id: checked-id }
              {
                creator: (get creator some-poll),
                question: (get question some-poll),
                yes: (get yes some-poll),
                no: (get no some-poll),
                start: (get start some-poll),
                end: (get end some-poll),
                open: false
              }
            )
            (ok true)
          )
        )
      )
    )
  )
)

;; ------------------------------------------------------------
;; READ-ONLY FUNCTIONS
;; ------------------------------------------------------------

(define-read-only (get-poll (poll-id uint))
  (map-get? polls { id: poll-id })
)

(define-read-only (get-vote (poll-id uint) (user principal))
  (map-get? votes { id: poll-id, voter: user })
)

(define-read-only (get-total-polls)
  (ok (var-get poll-count))
)
