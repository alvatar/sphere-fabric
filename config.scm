(sphere: "fabric")
(dependencies:
 (algorithm/compare (include
                     (core: base-macros)))
 (algorithm/list-extra (include
                        (core: base-macros))
                       (load
                        (= algorithm/list)))
 (algorithm/sort-merge (include
                        (core: base-macros)))
 (algorithm/stream (include
                    (= structure/stream-macros))
                   (load
                    (= structure/stream)))
 (algorithm/stream-extra (include
                          (= structure/stream-macros))
                         (load
                          (= structure/stream)
                          (= algorithm/stream)))
 (algorithm/random (load
                    (= algorithm/list)))
 (algorithm/vector (include
                    (core: base-macros)))
 (structure/stream (load
                    (= algorithm/list)))
 (structure/multi-dimensional-array (load
                                     (= algorithm/list))))
