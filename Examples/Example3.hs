module Examples.Example3 where

import Examples.Example2(Person(..))
import Examples.Utils(testC)
import Graphics.UI.VE
import ErrVal


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

test = testC (mkVE :: VE ConstE Team)