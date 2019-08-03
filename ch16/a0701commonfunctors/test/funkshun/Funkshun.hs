import ReplaceExperiment

main :: IO ()
main = do
  putStr "replaceWithP' lms: "
  print (replaceWithP' lms)

  putStr "liftedReplace lms: "
  print (liftedReplace lms)

  putStr "liftedReplace' lms: "
  print (liftedReplace' lms)

  putStr "twiceLifted lms:  "
  print (twiceLifted lms)

  putStr "twiceLifted' lms: "
  print (twiceLifted' lms)

  putStr "thriceLifted lms: "
  print (thriceLifted lms)

  putStr "thriceLifted' lms: "
  print (thriceLifted' lms)
