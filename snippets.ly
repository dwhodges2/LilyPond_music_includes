\version "2.18.2"

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Borrowed functions and other code %%%%%

%%% \changePitch

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% version 2013/05/11
%% for lilypond 2.18 or higher
%% last changes : - \changePitch : a single s as the last event of newnotes parameter
%%                   will give you the ending rests of the pattern (if there), and two s
%%                   also the beginning ones. If pattern ends with a note, a single 
%%                   ending s will have no effects.
%%                - Is now \language independant (no more #{ c #})
%%                - new algorithm for make-notes-list, change-pitch
%%                  and has-notes? (shorter and safer)
%%
%%%%%%%%%%%%%%%%%%%%%%%%% some utility functions %%%%%%%%%%%%%%%%%%%%%

#(define (name-of music)
 (ly:music-property music 'name))

#(define (has-notes? music)
"Return true if there is at least one note in `music, false otherwise."
 (or (eq? (name-of music) 'NoteEvent)
     (let ((e (ly:music-property music 'element)))
        (and (ly:music? e) 
             (has-notes? e)))
     (let loop ((es (ly:music-property music 'elements)))
        (and (pair? es)
             (or (has-notes? (car es))
                 (loop (cdr es)))))))
 
%% An EventChord is sometimes used as a wrapper in Lilypond, so we have to check
%% if a chord is a standard chord with notes. We could have used has-notes? but
%% this version is perhaps more efficient. 
%% Optional events name like 'RestEvent can be included.
#(define (note-or-chord? music . otherEvent)
"Is music a note or a chord with notes ?"
(let ((name (name-of music)))
 (or (memq name (cons 'NoteEvent otherEvent))
     (and (eq? name 'EventChord)  ; have this chord at least one note ? 
          (let loop ((es (ly:music-property music 'elements)))
             (and (pair? es)
                  (or (eq? (name-of (car es)) 'NoteEvent)
                      (loop (cdr es)))))))))

#(define (expand-q-chords music); for q chords : see chord-repetition-init.ly
(expand-repeat-chords! (list 'rhythmic-event) music))

#(define (clean-music mus)
"Try to reduce the number of sequential music"
(let ((name (ly:music-property mus 'name)))
  (cond
    ((eq? name 'SequentialMusic)
       (ly:music-set-property! mus 'elements (fold-right
          (lambda (evt prev-list)
            (if (eq? (name-of evt) 'SequentialMusic)
              (append (ly:music-property (clean-music evt) 'elements) prev-list)
              (cons (clean-music evt) prev-list)))
          '()
          (ly:music-property mus 'elements))))
    ((eq? name 'SimultaneousMusic)
       (ly:music-set-property! mus 'elements
                (map clean-music (ly:music-property mus 'elements))))
    ((memq name (list 'RelativeOctaveMusic 'UnrelativableMusic))
         (ly:music-set-property! mus 'element (clean-music
                  (ly:music-property mus 'element)))))
 mus))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% changePitch %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define cPInsertInPattern (gensym))
#(define cPSamePitch (gensym))
#(define cPSamePitchEnd (gensym))
#(define cPPatternEnd (gensym))

#(define (make-notes-list music)
"Make a list with each element will be of one of these types :
  1- a note, a chord, a rest
  2- an integer, indicating the number of notes to skip in pattern ( The user will
     indicate that, by a corresponding number of skips (s or \\skip) in `newnotes 
     parameter of \\changePitch )
  3- a list of musics, to be inserted between 2 notes of pattern, and added with
     the \\insert function, inside `newnotes"
(let ((res '())     ; the list to fill
      (prev #f))
  (define (fill-notes-list evt)
    (let ((tags (ly:music-property evt 'tags))
          (name (name-of evt)))
      (cond
        ((memq cPInsertInPattern tags)             ; a music added by \insert
            (ly:music-set-property! evt 'tags
                          (delq cPInsertInPattern tags))     ; remove the tag
            (if (integer? prev)(set! res (cons prev res)))
            (set! prev (if (pair? prev)(cons evt prev)(list evt))))  ; a list
         ((memq name (list 'SkipEvent 'SkipMusic))
            (if (pair? prev)(set! res (cons prev res))) ; keep the reverse order
            (set! prev (if (integer? prev) (1+ prev) 1)))
       ; ((memq name (list 'EventChord 'NoteEvent 'RestEvent))
         ((note-or-chord? evt 'RestEvent) ; a note, a chord, or a rest
            (if (or (pair? prev)(integer? prev))(set! res (cons prev res)))
            (set! prev evt)
            (set! res (cons evt res)))
         (else
          (let ((elt (ly:music-property evt 'element))
                (elts (ly:music-property evt 'elements)))
            (if (ly:music? elt) (fill-notes-list elt))
            (if (pair? elts)(for-each fill-notes-list elts)))))))
(fill-notes-list music)
(if (or (pair? prev)(integer? prev))(set! res (cons prev res)))
(reverse res)))

%%%%%%%%%%%%  used inside the inner function change-one-note
#(define (copy-duration from to)  ; from and to as EventChord or NoteEvent
(let ((max-dur #f)); in theory, 2 notes in a chord can have a different duration
  (music-map (lambda (x)            ; get main duration from `from
              (let ((dur (ly:music-property x 'duration)))
               (if (and (ly:duration? dur)
                        (or (not max-dur)
                            (ly:duration<? max-dur dur))); take the greater
                 (set! max-dur dur))
                 x))
              from)
  (music-map (lambda (x)            ; set duration to duration of `to
               (if (ly:duration? (ly:music-property x 'duration))
                  (ly:music-set-property! x 'duration max-dur))
               x)
             to)))

#(define (copy-arti from to) ; from and to as EventChord or NoteEvent
(let* ((es-from (ly:music-property from 'elements))
       (es-to (ly:music-property to 'elements))
       (arti-from (if (null? es-from) 
                    (ly:music-property from 'articulations)
                    (filter 
                      (lambda(x)
                        (not (ly:duration? (ly:music-property x 'duration))))
                      es-from))))
  (if (null? es-to)                       ; NoteEvent
    (ly:music-set-property! to 'articulations  
              (append (ly:music-property to 'articulations) arti-from))
    (ly:music-set-property! to 'elements  ; EventChord
              (append es-to arti-from)))
  ; copy also 'tags and 'to-relative-callback            
  (ly:music-set-property! to 'tags 
    (append (ly:music-property from 'tags)(ly:music-property to 'tags)))
   (if (null? es-to) 
      (ly:music-set-property! to 'to-relative-callback 
          (ly:music-property from 'to-relative-callback))
      (begin
        (ly:music-set-property! to 'to-relative-callback 
            ly:music-sequence::event-chord-relative-callback)
        (ly:music-set-property! (car es-to) 'to-relative-callback
            (ly:music-property from 'to-relative-callback))))
    ))

%% del-arti is called for all notes but the first of a \samePitch section. 
#(define (del-arti note-or-chord)
(ly:music-set-property! note-or-chord 'articulations '())
(ly:music-set-property! note-or-chord 'elements 
  (filter (lambda(x) 
              (and (ly:duration? (ly:music-property x 'duration))
                   (ly:music-set-property! x 'articulations '())))
          (ly:music-property note-or-chord 'elements))) ; can be empty
(music-map  ;; del all caution accidentals
  (lambda(x)(if (eq? (name-of x) 'NoteEvent) (begin
               (ly:music-set-property! x 'force-accidental #f)
               (ly:music-set-property! x 'cautionary #f)))
             x)
  note-or-chord))

#(define (change-pitch pattern newnotes)
"The scheme function of \\changePitch, `pattern and `newnotes as music."
(let ((seq-list '())           ; list of transformed patterns
      (skip-notnote-event? #f) ; #t if a \skip or an \insert is found in newnotes
      (same-pitch-section? #f) ; #t if we are in the 2nd pattern note of a\samePitch section
      ;(dummy-note #{ c4 #})    ; \language dependant -:(
      (dummy-note (make-music 'NoteEvent ; to avoid pbs with pattern without any notes
                              'duration (ly:make-duration 2 0 1) ;
                              'pitch (ly:make-pitch -1 0 0))) ;
      (pattern2 #{ $pattern \tag #cPPatternEnd s4 #}) ; to detect the end of pattern
      (last-notes-list #f))    ; buffer 
 (set! seq-list (cdr  ; skip dummy notes
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; loop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let loop ((notes-list (cons dummy-note (make-notes-list newnotes))); see make-notes-list
             (pat-list (cons dummy-note (circular-list pattern2)))
             (res '())) ; the list to fill
    (if (or (null? notes-list)(null? pat-list)) ; pat-list may be a regular list in the loop
      (reverse res)               ;;;;;; return the list in the right order
      (let ((x (car notes-list))  ;;;;;; go deeper, taking 1st elt of each lists
            (evt (ly:music-deep-copy (car pat-list))))
       (cond
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
        ((pair? x)                   ; x is a list of musics, added with \insert in newnotes
          (set! skip-notnote-event? #t)      ; for events between 2 pattern notes
          (set! last-notes-list notes-list)  ; for section "else" of this cond statement
          (loop (cdr notes-list) pat-list (append x res))) ; append x to res
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ((note-or-chord? evt)         ; evt in pattern is a note or a chord (with notes)
          (set! last-notes-list notes-list)  ; for section "else" later
          (cond
            ((ly:music? x)  ;;;;;; the elt of notes-list is a note or a chord
               (if same-pitch-section? ; x is used several times. Copy arti of x only to the
                 (set! x (del-arti (ly:music-deep-copy x)))) ; 1st note of \samePitch section
               (copy-duration evt x)  ; evt = from, x = to
               (copy-arti evt x)
               (let ((tags (ly:music-property x 'tags)))
                 (cond               ; are we in a \samePitch section ?
                   ((memq cPSamePitch tags)    ; yes, first,remove the tag
                      (ly:music-set-property! x 'tags (delq cPSamePitch tags))
                      (set! same-pitch-section? #t)) ; then set the flag        
                   ((memq cPSamePitchEnd tags) ; last note of \samePitch
                      (ly:music-set-property! x 'tags (delq cPSamePitchEnd tags))
                      (set! same-pitch-section? #f))))      ; unset the flag
               (set! skip-notnote-event? #f); stop deletion of not-notes event.
               (if same-pitch-section?
                   (loop notes-list (cdr pat-list)(cons x res))    
                   (loop (cdr notes-list)(cdr pat-list)(cons x res)))) ; next new note              
            ((integer? x)   ;;;;;; user want to skip over the current evt note. We also
               (set! skip-notnote-event? x) ; don't add any events bis next pattern note
               ;; (format #t "x : ~a\n" x)  ; for testing
               (cond ((= x 1)                                      ; only one s
                        (loop (cdr notes-list)(cdr pat-list) res)) ; next notes-list elt
                     (else                                         ; several successive s
                        (set-car! notes-list (1- x))               ; for the next loop
                        (loop notes-list (cdr pat-list) res))))))  ; the next evt only
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        
        ((or (ly:music-property evt 'duration #f)  ; current evt in pattern is not a note
             (not (has-notes? evt)))
           (cond ((memq cPPatternEnd (ly:music-property evt 'tags)) ; last evt of pattern
                    (let ((x (car notes-list)))
                      (if (and (integer? x)
                                (or (null? (cdr notes-list))        ; last elt ?
                                    (and (null? (cddr notes-list))  ; 2nd to last and last is
                                         (pair? (car (cdr notes-list)))))) ; a \insert section
                        (cond 
                           ((= x 1)
                              (set! skip-notnote-event? x)
                              (loop (cdr notes-list) (cdr pat-list) res))
                           (else 
                              (set-car! notes-list (1- x))
                              (loop notes-list (cdr pat-list) res))))
                        (loop notes-list (cdr pat-list) res))) ;; ignores evt
                  (skip-notnote-event? (loop notes-list (cdr pat-list) res))
                  (else (loop notes-list (cdr pat-list)(cons evt res)))))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (else
          (let ((e (ly:music-property evt 'element))
                (es (ly:music-property evt 'elements))
                (empty? #f)) ; don't add to res if #t
           (if (ly:music? e)
             (let ((new-e (loop notes-list (list e) '())))
               (ly:music-set-property! evt 'element
                 (case (length new-e)
                   ((0) (set! empty? #t)
                        new-e)
                   ((1)(car new-e))
                   (else (make-sequential-music new-e))))))
           (if (pair? es)
             (let ((new-es (loop notes-list es '())))
               (ly:music-set-property! evt 'elements new-es)
               (set! empty? (and empty? (null! new-es))))) ; #t if both empty 
           
           (let ((next-new-notes (if (or same-pitch-section? 
                                         (and (integer? skip-notnote-event?)
                                              (> skip-notnote-event? 1)))
                                    last-notes-list 
                                    (cdr last-notes-list))))
              (if empty? (loop next-new-notes (cdr pat-list) res)
                         (loop next-new-notes (cdr pat-list) (cons evt res))))))))))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end loop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
 (let ((relativize (lambda(m)
        (let* ((clean-newnotes (clean-music newnotes))
               (name (name-of clean-newnotes)))
            (if (memq name (list 'RelativeOctaveMusic 'UnrelativableMusic))
               (make-music name 'element m)
               m)))))
     (case (length seq-list)
        ((0) (make-music 'Music 'void #t))
        ((1) (relativize (car seq-list)))
        (else (relativize (clean-music (make-sequential-music seq-list))))))))
        
changePitch = #(define-music-function (parser location pattern newnotes)
                                                          (ly:music? ly:music?)
"Change each notes in `pattern by the notes (or rests) given in `newnotes.
If count of events doesn't match, pattern is duplicated repeatedly or truncate."
(let* ((expand-q (lambda (music) (expand-repeat-chords!
			    (cons 'rhythmic-event (ly:parser-lookup parser '$chord-repeat-events))
			    music)))
       (pattern (expand-q pattern))
       (newnotes (expand-q newnotes)))
 (change-pitch pattern newnotes)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% enhancement functions, working with \changePitch pattern newnotes

samePitch = #(define-music-function (parser location music) (ly:music?)
"Inside the `pattern parameter of the \\changePitch function, all notes grouped 
by this function will have the same pitch, according to the current note of
`newnotes parameter of \\changePitch."
(let((not-first? #f)
     (last-note #f))
  (map-some-music
    (lambda (x)
      (cond
        ((note-or-chord? x)
           (if not-first?     ; set all pitches to the pitch of the first note
             (ly:music-set-property! x 'to-relative-callback 
                (lambda (x p)                    ; set pitch to the prev value
                    (ly:prob-set-property! x 'pitch p)
                    p))
             (set! not-first? x)) ; do nothing for first note
           (ly:music-set-property! x 'tags (cons
                   cPSamePitch  ; add tag cPSamePitch to x
                   (ly:music-property x 'tags)))
           (set! last-note x)   ; save the note x
           x)
        (else #f)))
    music)
  (if last-note              ; the last saved EventChord
     (ly:music-set-property! last-note 'tags (cons
           cPSamePitchEnd    ; add cPSamePitchEnd tag, delete cPSamePitch tag
           (delq cPSamePitch (ly:music-property last-note 'tags)))))
  music))

%% this function should be no more needed, as copy-arti should avoid pbs
%% in relative mode and \samePitch
absolute = #(define-music-function (parser location music) (ly:music?)
"Make `music unrelativable. To use inside a \\samePitch function in relative
mode."
(make-music 'UnrelativableMusic 'element music))

insert = #(define-music-function (parser location music) (ly:music?)
"Using this function inside the `newnotes parameter of the \\changePitch
function, allow you to insert and remplace by `music, all music between one note
and his following, in the `pattern parameter of \\changePitch, ."
#{ \tag #cPInsertInPattern $music #})

%%%%%%%
#(define (n-copy n music)
(cond
  ((> n 1)(make-sequential-music 
            (map (lambda (x)(ly:music-deep-copy music))
                 (make-list n))))
  ((= n 1) music)
  (else (make-music 'Music 'void #t))))

nCopy = #(define-music-function (parser location n music)(integer? ly:music?)
(n-copy n music))

%% same effect as { \repeat unfold n s } but \nSkip works inside the `newnotes
%% parameter of \changePitch.
nSkip = #(define-music-function (parser location n)(integer?)
"Return \\skip \\skip \\skip ... n times."
#{ \nCopy #n s #})
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% shortcuts
% default values for patI and patII, if the user do not define
% them, before using \cPI and \cPII
% patI ={ c8. c16 }      % not \language independant
patI = #(make-music 'SequentialMusic 'elements (list  
          (make-music 'NoteEvent 'duration (ly:make-duration 3 1 1) 
                                 'pitch (ly:make-pitch -1 0 0))
          (make-music 'NoteEvent 'duration (ly:make-duration 4 0 1)
                                 'pitch (ly:make-pitch -1 0 0)))) 
% patII = { c4. c8 }
patII = #(make-music 'SequentialMusic 'elements (list  
          (make-music 'NoteEvent 'duration (ly:make-duration 2 1 1) 
                                 'pitch (ly:make-pitch -1 0 0))
          (make-music 'NoteEvent 'duration (ly:make-duration 3 0 1)
                                 'pitch (ly:make-pitch -1 0 0)))) 


cPI = #(define-music-function (parser location newnotes) (ly:music?)
#{ \changePitch \patI $newnotes #})

cPII = #(define-music-function (parser location newnotes) (ly:music?)
#{ \changePitch \patII $newnotes #})

#(define cP changePitch)




%%%%%%%%%%%%%%%%

%%%  \copyArticulations

%% version Y/M/D = 2015/09/01 for lilypond 2.16 or higher
%% LSR = http://lsr.di.unimi.it/LSR/Item?id=769
  

#(use-modules (ice-9 receive)) %% for the use of receive

#(define skipCurrentArticulationTag (gensym))
#(define notCopyArticulationsTag (gensym))


#(define (defined-music? music)
   (not (eq? 'Music (ly:music-property music 'name))))

#(define (name-of music)
 (ly:music-property music 'name))

#(define (noteEvent? music)
(eq? (name-of music) 'NoteEvent))
 
#(define (has-notes? music)
"Return true if there is at least one note in `music, false otherwise."
 (or (noteEvent? music)
     (let ((e (ly:music-property music 'element)))
        (and (ly:music? e) 
             (has-notes? e)))
     (let loop ((es (ly:music-property music 'elements)))
        (and (pair? es)
             (or (has-notes? (car es))
                 (loop (cdr es)))))))

#(define (make-arti-list pattern)
"Make a list of articulations "
 (let ((res '()))     ; the list to fill
   (for-some-music    ; see music-functions.scm
     (lambda (evt)
       (case (name-of evt)
         ((EventChord)
           (receive (notes others)
             (partition noteEvent? (ly:music-property evt 'elements))
             (if (pair? notes)(set! res (cons others res))) ; keeps not-note-events
             res))                                          ; stop process         
         ((NoteEvent)
             (set! res (cons (ly:music-property evt 'articulations) res))
             res)
         (else #f)))
     pattern)
   (reverse res)))

%% use (ly:music-deep-copy music) for a direct use of copy-articulations
#(define (copy-articulations pattern music)
"Copy articulations of `pattern recursively in each notes of music."
(if (not (has-notes? pattern))              ; avoid endless loops ...
  music
  (let* ((new-arti (make-arti-list pattern))
         (pointer-list new-arti)
         (current-arti (lambda ()
            (let ((res (car pointer-list)))
              (set! pointer-list (cdr pointer-list))
              (if (null? pointer-list)(set! pointer-list new-arti))
              res))))
    (music-filter
      defined-music?                        ; deletes all (make-music 'Music)
      (map-some-music ; see music-functions.scm
        (lambda(evt)
          (let ((tags (ly:music-property evt 'tags)))
             (cond 
               ((memq skipCurrentArticulationTag tags); yes => evt = <>
                     (current-arti)                ; ignores return
                     (make-music 'Music))          ; will be deleted...
               ((memq notCopyArticulationsTag tags)
                   (ly:music-set-property! evt 'tags ; just remove the tag
                        (delq notCopyArticulationsTag tags))
                   evt)
               ((noteEvent? evt)
                   (let ((arti (ly:music-property evt 'articulations)))
                     (ly:music-set-property! evt 'articulations
                       (append arti (current-arti)))
                     evt))
               ((eq? 'EventChord (name-of evt))
                   (let ((elts (ly:music-property evt 'elements)))
                     (and 
                      (pair? (filter noteEvent? elts))
                      (begin
                       (ly:music-set-property! evt 'elements
                                 (append elts (current-arti)))
                       evt))))
               ;(else (and (ly:music-property evt 'duration #f) ; tuplets now can have
               ;           evt)))))                             ; a duration ! 
               (else #f))))	                                    ; continue loop
          (expand-repeat-chords! (list 'rhythmic-event) music))))))
 
copyArticulations = #(define-music-function (parser location pattern music)
                                                          (ly:music? ly:music?)
"Copy articulations of `pattern recursively in each notes of music."
 (copy-articulations pattern music))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% enhancement functions, working inside the music parameter 
%% of \copyArticulations
                         %%%%%%%
notCopyArticulations = 
#(define-music-function (parser location music)(ly:music?)
"Add a special tag in 'tags of all notes and chords, that will forbid 
articulations to be copied to these events."
(map-some-music
 (lambda(evt)
   (cond ((memq (name-of evt) '(NoteEvent EventChord))
            (ly:music-set-property! evt 'tags (cons
               notCopyArticulationsTag (ly:music-property evt 'tags)))
            evt)
          (else #f))) ; go deeper
 music))

skipArti = \tag #skipCurrentArticulationTag <>

nSkipArti = #(define-music-function (parser location n)(integer?)
"Return \\skipArti \\skipArti \\skipArti ... n times."
(if (< n 2)
  skipArti
 (make-sequential-music (make-list n skipArti))))

#(define (skip-tied-notes mus)
(let ((prev-was-tied #f))
  (map-some-music
   (lambda(evt)
     (let ((name (name-of evt)))
       (cond 
        ((or (eq? name 'NoteEvent)
             (and (eq? name 'EventChord)
                  (has-notes? evt)))
         (if prev-was-tied (ly:music-set-property! evt 'tags 
                             (cons notCopyArticulationsTag (ly:music-property evt 'tags))))
         (map-some-music
          (lambda(x)
            (set! prev-was-tied (and (eq? (name-of x) 'TieEvent) x))
            prev-was-tied)  ; stop if true
          evt))
        (else #f))))
   (expand-repeat-chords! (list 'rhythmic-event) mus))))

skipTiedNotes = #(define-music-function (parser location music)(ly:music?)
"Add a special tag in 'tags of second tied notes to forbid articulations to be 
copied."
(skip-tied-notes music))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                  %% adding bracket to chords %%

%% \copyArticulations { c\arpeggio } { \arpeggioBracket \music } works well,
%% but we want brackets only to chords with at least 2 notes (not to notes alone), and not to tied
%% chords. So we need special functions.

#(define (add-arp mus)
(map-some-music
 (lambda(evt)
   (let ((tags (ly:music-property evt 'tags)))
     (cond 
      ((memq notCopyArticulationsTag tags)
       (ly:music-set-property! evt 'tags ; just remove the tag
         (delq notCopyArticulationsTag tags))
       evt)
      ((eq? 'EventChord (name-of evt))
       (let ((ees (ly:music-property evt 'elements)))
         (if (let loop ((n 0)   ;; takes chords with at least 2 notes
                        (notes ees))
               (or (> n 1)
                   (and (pair? notes)
                        (loop (if (eq? 'NoteEvent (name-of (car notes)))
                                  (1+ n) n)
                              (cdr notes)))))              
             (ly:music-set-property! evt 'elements
               (append ees (list (make-music 'ArpeggioEvent)))))
         evt))
      ;(else (and (ly:music-property evt 'duration #f) evt)))))
      (else #f))))
 mus))

#(define (braketify-chords mus) ;; this scheme function is used in "arranger.ly"
   #{ \arpeggioBracket $(add-arp (skip-tied-notes mus)) #})
   

braketifyChords = #(define-music-function (parser location music) (ly:music?)
"Adds brackets to chords with at least 2 notes."
  (braketify-chords music))
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                      %% shortcuts %%

artiI = c-.              % default values for artiI and artiII, if the user does          
artiII = {c( c) c-. c-.} % not define them, before using \cAI and \cAII

cAI = #(define-music-function (parser location music) (ly:music?)
#{ \copyArticulations \artiI $music #})

cAII = #(define-music-function (parser location music) (ly:music?)
#{ \copyArticulations \artiII $music #})

#(define cA copyArticulations)
#(define notCA notCopyArticulations)





%%%%%%%%%%%%%%%%%%%%%%

%%%    \makeOctaves

%%  http://lsr.di.unimi.it/LSR/Item?id=445

%LSR by Jay Anderson.
%modyfied by Simon Albrecht on March 2014.
%=> http://lilypond.1069038.n5.nabble.com/LSR-445-error-td160662.html

#(define (octave-up m t)
 (let* ((octave (1- t))
      (new-note (ly:music-deep-copy m))
      (new-pitch (ly:make-pitch
        octave
        (ly:pitch-notename (ly:music-property m 'pitch))
        (ly:pitch-alteration (ly:music-property m 'pitch)))))
  (set! (ly:music-property new-note 'pitch) new-pitch)
  new-note))

#(define (octavize-chord elements t)
 (cond ((null? elements) elements)
     ((eq? (ly:music-property (car elements) 'name) 'NoteEvent)
       (cons (car elements)
             (cons (octave-up (car elements) t)
                   (octavize-chord (cdr elements) t))))
     (else (cons (car elements) (octavize-chord (cdr elements ) t)))))

#(define (octavize music t)
 (if (eq? (ly:music-property music 'name) 'EventChord)
       (ly:music-set-property! music 'elements (octavize-chord
(ly:music-property music 'elements) t)))
 music)

makeOctaves = #(define-music-function (parser location arg mus) (integer? ly:music?)
 (music-map (lambda (x) (octavize x arg)) (event-chord-wrap! mus)))



%%%%%%%%%%%%%%%%%%
%%%%  GIT  COMMANDS    %%%%  
%%%%%%%%%%%%%%%%%%

%%  borrowed from 
%%  http://lilypondblog.org/2014/04/printing-git-versioning-info-in-a-score/
%%
%% Note: In Frescobaldi, must save file before compiling, 
%% otherwise it reads git info from a tmp file that has none. :(


#(use-modules (ice-9 popen))
#(use-modules (ice-9 rdelim))

% Read a command, execute it as a shell command and return
% the command's result as a string
% NOTE: This function only reads the first line of the command
% and returns the whole result as one single string
% (in case of multiline results the caller has to split that)
#(define (strsystem_internal cmd)
   (let* ((port (open-input-pipe cmd))
          (str (read-delimited "" port)))
     (close-pipe port)
     (if (eof-object? str) "" str)))

% execute a Git command and return its result as a \markup \column
% with one or more lines in it. If the result is empty an error
% will be raised about unexpected #<eof>
% Pass the function a git command without "git "
#(define-markup-command (gitCommand layout props cmd) (markup?)
   (let* ((result (string-split
                   (strsystem_internal (string-append "git " cmd))
                   #\newline)))
       (interpret-markup layout props
         #{ \markup \column #result #})))


%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Actual commands

% Print the (short) committish of the latest commit
gitCommitish = \markup { \gitCommand "rev-parse --short HEAD" }

% Print the oneline commit message of the latest commit
gitCommit = \markup { \gitCommand "log --oneline HEAD^1..HEAD" }

% Print date and time of the latest commit
gitDateTime = \markup { \gitCommand "show -s --format=%ci HEAD" }

gitAuthor = \markup { \gitCommand "show -s --format=%an HEAD" }

gitEmail = \markup { \gitCommand "show -s --format=%ae HEAD" }

% Print the (short) committish of the latest commit
gitParentCommittish = \markup { \gitCommand "rev-parse --short HEAD^1" }

% Print the oneline commit message of the latest commit
gitParentCommit = \markup { \gitCommand "log --oneline HEAD~2..HEAD~1" }


% Print the branch the repository is currently on
gitBranch = \markup { \gitCommand "rev-parse --abbrev-ref HEAD" }

% Print the number of commits that lead to the
% current commit. This may not be reliable because
% of the counting of merge commits
gitRevisionNumber = \markup { \gitCommand "log --oneline | wc -l" }

% Return ##t if the repository is clean, i.e. if it
% doesn't have any uncommitted changes
#(define (gitIsClean)
   (eq? "" (strsystem_internal  "git status --porcelain")))


%% Use \hasUncommittedChanges to add a * after commit (or anything else)
#(define-markup-command (hasUncommittedChanges layout props) ()
   (if (gitIsClean)
   (interpret-markup layout props
       #{ \markup \bold "" #})
   (interpret-markup layout props
       #{ \markup \bold "*" #})))




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% "Verbose" commands

% Print the full commit message of the latest commit
gitFullCommit = \markup { \gitCommand "log --pretty=full HEAD^1..HEAD" }

% Print a full diff between current HEAD and the working tree
gitDiff = \markup { \gitCommand "diff" }



%%%%%%%%%%%

