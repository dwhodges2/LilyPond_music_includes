
%%%%%%%%   Functions   %%%%%%%%%

  redNote =
  #(define-music-function (parser location)()
   #{
     \once \override NoteHead.color = #red
     \once \override Stem.color = #red
   #})


% Different notehead styles

slashOn = \override NoteHead.style = #'slash
slashOff = \revert NoteHead.style

crOn = \override NoteHead.style = #'cross
crOff = \revert NoteHead.style

abriendo = \downbow
cerrando = \upbow

%%%% Variables  %%%
date = #(strftime "%m-%d-%Y" (localtime (current-time)))




