module Talk.Example3 where

import Talk.Example2
import VE
import ErrVal
import Test


data Team = Team {
    t_leader :: Person,
    t_followers :: [Person]
} deriving (Show)

instance HasVE Team
  where
    mkVE = mapVE toStruct fromStruct
        (   label "Leader" mkVE
        .*. label "Followers"  (listVE st_name mkVE)
        )
      where
        toStruct (a,b) = eVal (Team a b)
        fromStruct (Team a b) = (a,b)
