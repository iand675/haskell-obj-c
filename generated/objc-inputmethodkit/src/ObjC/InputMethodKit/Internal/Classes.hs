{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.InputMethodKit.Internal.Classes (
    module ObjC.InputMethodKit.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- IMKInputController ----------

-- | IMKInputController
--
-- The basic class that controls input on the input method side.
--
-- IMKInputController implements fully implements the protocols defined above.  Typically a developer does not override this class, but provides a delegate object that implements the methods that developer is interested in.  The IMKInputController versions of the protocol methods check if the delegate object implements a method, and  call the delegate version if it exists.
--
-- The IMKServer class which is allocated in an input method's main function creates a controller class for each input session created by a client application. Therefore for every input session there is a corresponding IMKInputController.
-- 
-- Phantom type for @IMKInputController@.
data IMKInputController

instance IsObjCObject (Id IMKInputController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IMKInputController"

class IsNSObject a => IsIMKInputController a where
  toIMKInputController :: a -> Id IMKInputController

instance IsIMKInputController (Id IMKInputController) where
  toIMKInputController = unsafeCastId

instance IsNSObject (Id IMKInputController) where
  toNSObject = unsafeCastId

-- ---------- IMKServer ----------

-- | IMKServer
--
-- This class manages input sessions.
--
-- An input method should create one and only one of these objects.  An IMKServer creates an NSConnection that can be connected to by input clients.  After a connection has been made an IMKServer manages communication between the client and the input method.  For each communication session the IMKServer will create an IMKInputController class as well as delegate classes for that controller.  Each controller object then serves as a proxy for the input session on the client side.  This means that input methods do not have to concern themselves with managing client sessions.  A given controller will only receive communication from a single session.
--
-- IMKServer's also will manage a basic candidate window for an input method.  See IMKCandidates.h to understand how to create a candidate window and associate the candidate window with the IMKServer object.
-- 
-- Phantom type for @IMKServer@.
data IMKServer

instance IsObjCObject (Id IMKServer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IMKServer"

class IsNSObject a => IsIMKServer a where
  toIMKServer :: a -> Id IMKServer

instance IsIMKServer (Id IMKServer) where
  toIMKServer = unsafeCastId

instance IsNSObject (Id IMKServer) where
  toNSObject = unsafeCastId

-- ---------- IMKCandidates ----------

-- | Phantom type for @IMKCandidates@.
data IMKCandidates

instance IsObjCObject (Id IMKCandidates) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IMKCandidates"

class IsNSResponder a => IsIMKCandidates a where
  toIMKCandidates :: a -> Id IMKCandidates

instance IsIMKCandidates (Id IMKCandidates) where
  toIMKCandidates = unsafeCastId

instance IsNSObject (Id IMKCandidates) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id IMKCandidates) where
  toNSResponder = unsafeCastId
