-----------------------------------------------------------------------------------------
{-| Module      : Tests.Common
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Tests.Common where

import HMinCaml.Common

unitID x = TypedIdentifier x UnitType                                               
integerID x = TypedIdentifier x IntegerType
floatID x = TypedIdentifier x FloatingPointType
booleanID x = TypedIdentifier x BooleanType