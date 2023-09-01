(lang dune 3.0)
(source (github smimram/learning-effect))
(authors "Samuel Mimram")
(maintainers "Samuel Mimram")

(generate_opam_files)

(package
 (name learning-effect)
 (synopsis "Use machine to learn audio effects")
 (description "Machine learning algorithms applied to audio effects.")
 (depends
   pulseaudio
   yojson
 )
)
