{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLStructType@.
module ObjC.Metal.MTLStructType
  ( MTLStructType
  , IsMTLStructType(..)
  , memberByName
  , members
  , memberByNameSelector
  , membersSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- memberByName:@
memberByName :: (IsMTLStructType mtlStructType, IsNSString name) => mtlStructType -> name -> IO (Id MTLStructMember)
memberByName mtlStructType  name =
withObjCPtr name $ \raw_name ->
    sendMsg mtlStructType (mkSelector "memberByName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- members@
members :: IsMTLStructType mtlStructType => mtlStructType -> IO (Id NSArray)
members mtlStructType  =
  sendMsg mtlStructType (mkSelector "members") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @memberByName:@
memberByNameSelector :: Selector
memberByNameSelector = mkSelector "memberByName:"

-- | @Selector@ for @members@
membersSelector :: Selector
membersSelector = mkSelector "members"

