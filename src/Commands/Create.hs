module Commands.Create where

import Utils (askQuestion, bail)

create :: IO ()
create = do
  txt <- askQuestion "What is the name of the application?"
  bail $ txt <> " -- coming soon!"