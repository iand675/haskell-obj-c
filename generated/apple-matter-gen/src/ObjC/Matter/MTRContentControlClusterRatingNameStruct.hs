{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentControlClusterRatingNameStruct@.
module ObjC.Matter.MTRContentControlClusterRatingNameStruct
  ( MTRContentControlClusterRatingNameStruct
  , IsMTRContentControlClusterRatingNameStruct(..)
  , ratingName
  , setRatingName
  , ratingNameDesc
  , setRatingNameDesc
  , ratingNameDescSelector
  , ratingNameSelector
  , setRatingNameDescSelector
  , setRatingNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- ratingName@
ratingName :: IsMTRContentControlClusterRatingNameStruct mtrContentControlClusterRatingNameStruct => mtrContentControlClusterRatingNameStruct -> IO (Id NSString)
ratingName mtrContentControlClusterRatingNameStruct =
  sendMessage mtrContentControlClusterRatingNameStruct ratingNameSelector

-- | @- setRatingName:@
setRatingName :: (IsMTRContentControlClusterRatingNameStruct mtrContentControlClusterRatingNameStruct, IsNSString value) => mtrContentControlClusterRatingNameStruct -> value -> IO ()
setRatingName mtrContentControlClusterRatingNameStruct value =
  sendMessage mtrContentControlClusterRatingNameStruct setRatingNameSelector (toNSString value)

-- | @- ratingNameDesc@
ratingNameDesc :: IsMTRContentControlClusterRatingNameStruct mtrContentControlClusterRatingNameStruct => mtrContentControlClusterRatingNameStruct -> IO (Id NSString)
ratingNameDesc mtrContentControlClusterRatingNameStruct =
  sendMessage mtrContentControlClusterRatingNameStruct ratingNameDescSelector

-- | @- setRatingNameDesc:@
setRatingNameDesc :: (IsMTRContentControlClusterRatingNameStruct mtrContentControlClusterRatingNameStruct, IsNSString value) => mtrContentControlClusterRatingNameStruct -> value -> IO ()
setRatingNameDesc mtrContentControlClusterRatingNameStruct value =
  sendMessage mtrContentControlClusterRatingNameStruct setRatingNameDescSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ratingName@
ratingNameSelector :: Selector '[] (Id NSString)
ratingNameSelector = mkSelector "ratingName"

-- | @Selector@ for @setRatingName:@
setRatingNameSelector :: Selector '[Id NSString] ()
setRatingNameSelector = mkSelector "setRatingName:"

-- | @Selector@ for @ratingNameDesc@
ratingNameDescSelector :: Selector '[] (Id NSString)
ratingNameDescSelector = mkSelector "ratingNameDesc"

-- | @Selector@ for @setRatingNameDesc:@
setRatingNameDescSelector :: Selector '[Id NSString] ()
setRatingNameDescSelector = mkSelector "setRatingNameDesc:"

