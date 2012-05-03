module Talk.Example3 where

import Talk.Example2
import UI
import ErrVal
import Test


data Team = Team {
    t_leader :: Person,
    t_followers :: [Person]
} deriving (Show)

instance HasUI Team
  where
    mkUI = mapUI toStruct fromStruct
        (   label "Leader" mkUI
        .*. label "Followers"  (listUI st_name mkUI)
        )
      where
        toStruct (a,b) = eVal (Team a b)
        fromStruct (Team a b) = (a,b)
