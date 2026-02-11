{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Context of a password authentication operation.
--
-- Generated bindings for @TKTokenPasswordAuthOperation@.
module ObjC.CryptoTokenKit.TKTokenPasswordAuthOperation
  ( TKTokenPasswordAuthOperation
  , IsTKTokenPasswordAuthOperation(..)
  , password
  , setPassword
  , passwordSelector
  , setPasswordSelector


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

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Password, which will be filled in by the system when 'finishWithError:' is called.
--
-- ObjC selector: @- password@
password :: IsTKTokenPasswordAuthOperation tkTokenPasswordAuthOperation => tkTokenPasswordAuthOperation -> IO (Id NSString)
password tkTokenPasswordAuthOperation  =
  sendMsg tkTokenPasswordAuthOperation (mkSelector "password") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Password, which will be filled in by the system when 'finishWithError:' is called.
--
-- ObjC selector: @- setPassword:@
setPassword :: (IsTKTokenPasswordAuthOperation tkTokenPasswordAuthOperation, IsNSString value) => tkTokenPasswordAuthOperation -> value -> IO ()
setPassword tkTokenPasswordAuthOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg tkTokenPasswordAuthOperation (mkSelector "setPassword:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @password@
passwordSelector :: Selector
passwordSelector = mkSelector "password"

-- | @Selector@ for @setPassword:@
setPasswordSelector :: Selector
setPasswordSelector = mkSelector "setPassword:"

