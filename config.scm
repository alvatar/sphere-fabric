(sphere: "fabric")
(dependencies:
 (algorithm/compare
  (include
   (core: base-macros)))
 (algorithm/list
  (include
   (core: base-macros)))
 (algorithm/list-extra
  (include
   (core: base-macros))
  (load
   (= algorithm/list)))
 (algorithm/shuffle
  (load
   (= algorithm/list)))
 (algorithm/sort-merge
  (include
   (core: base-macros)))
 (algorithm/stream
  (include
   (= structure/stream-macros))
  (load
   (= structure/stream)))
 (algorithm/stream-extra
  (include
   (= structure/stream-macros))
  (load
   (= structure/stream)
   (= algorithm/stream)))
 (algorithm/random
  (include
   (core: base-macros)
   (core: match-macros))
  (load
   (= algorithm/sort-merge)
   (= algorithm/stream)))
 (algorithm/u8vector
  (include
   (core: base-macros)))
 (algorithm/vector
  (include
   (core: base-macros)))
 (structure/stream
  (load
   (= algorithm/list)))
 (structure/multi-dimensional-array
  (load
   (= algorithm/list))))
