{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IMKServer
--
-- This class manages input sessions.
--
-- An input method should create one and only one of these objects.  An IMKServer creates an NSConnection that can be connected to by input clients.  After a connection has been made an IMKServer manages communication between the client and the input method.  For each communication session the IMKServer will create an IMKInputController class as well as delegate classes for that controller.  Each controller object then serves as a proxy for the input session on the client side.  This means that input methods do not have to concern themselves with managing client sessions.  A given controller will only receive communication from a single session.
--
-- IMKServer's also will manage a basic candidate window for an input method.  See IMKCandidates.h to understand how to create a candidate window and associate the candidate window with the IMKServer object.
--
-- Generated bindings for @IMKServer@.
module ObjC.InputMethodKit.IMKServer
  ( IMKServer
  , IsIMKServer(..)
  , initWithName_bundleIdentifier
  , initWithName_controllerClass_delegateClass
  , bundle
  , paletteWillTerminate
  , lastKeyEventWasDeadKey
  , initWithName_bundleIdentifierSelector
  , initWithName_controllerClass_delegateClassSelector
  , bundleSelector
  , paletteWillTerminateSelector
  , lastKeyEventWasDeadKeySelector


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

import ObjC.InputMethodKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a IMKServer from information in the bundle's Info.plist.
--
-- This method will look into the info.plist for a controller class and delegate class.  The class names will be loaded, no classes will be instantiated.  Additionally, an NSConnection will be allocated and registered with the name parameter.
--
-- ObjC selector: @- initWithName:bundleIdentifier:@
initWithName_bundleIdentifier :: (IsIMKServer imkServer, IsNSString name, IsNSString bundleIdentifier) => imkServer -> name -> bundleIdentifier -> IO RawId
initWithName_bundleIdentifier imkServer  name bundleIdentifier =
withObjCPtr name $ \raw_name ->
  withObjCPtr bundleIdentifier $ \raw_bundleIdentifier ->
      fmap (RawId . castPtr) $ sendMsg imkServer (mkSelector "initWithName:bundleIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_bundleIdentifier :: Ptr ())]

-- | Creates an IMKServer using the parameters.
--
-- This method creates an IMKServer object without attempting to examine the bundle instead the class names provided as parameters are used to create input controller objects and delegate objects.
--
-- ObjC selector: @- initWithName:controllerClass:delegateClass:@
initWithName_controllerClass_delegateClass :: (IsIMKServer imkServer, IsNSString name) => imkServer -> name -> Class -> Class -> IO RawId
initWithName_controllerClass_delegateClass imkServer  name controllerClassID delegateClassID =
withObjCPtr name $ \raw_name ->
    fmap (RawId . castPtr) $ sendMsg imkServer (mkSelector "initWithName:controllerClass:delegateClass:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (unClass controllerClassID), argPtr (unClass delegateClassID)]

-- | Returns an NSBundle for the input method.
--
-- If the IMKServer contains a bundle identifier the NSBundle is created from that.  Otherwise, the bundle  is created for the main bundle.  The returned NSBundle is an autoreleased object.
--
-- ObjC selector: @- bundle@
bundle :: IsIMKServer imkServer => imkServer -> IO (Id NSBundle)
bundle imkServer  =
  sendMsg imkServer (mkSelector "bundle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Call this before terminating a palette IM.
--
-- Palettes need to be able to terminate.  When this method is called the IMKServer will notify each client of the palette that			 the palette is about to terminate.  The palette can terminate safely if a value of YES is returned.  If the caller of this method is not			 an input method of type palette an exception will be thrown.
--
-- If the method returns NO the palette should not terminate.
--
-- ObjC selector: @- paletteWillTerminate@
paletteWillTerminate :: IsIMKServer imkServer => imkServer -> IO Bool
paletteWillTerminate imkServer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg imkServer (mkSelector "paletteWillTerminate") retCULong []

-- | Returns a BOOL indicating whether or not the last key press was a dead key.
--
-- ObjC selector: @- lastKeyEventWasDeadKey@
lastKeyEventWasDeadKey :: IsIMKServer imkServer => imkServer -> IO Bool
lastKeyEventWasDeadKey imkServer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg imkServer (mkSelector "lastKeyEventWasDeadKey") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:bundleIdentifier:@
initWithName_bundleIdentifierSelector :: Selector
initWithName_bundleIdentifierSelector = mkSelector "initWithName:bundleIdentifier:"

-- | @Selector@ for @initWithName:controllerClass:delegateClass:@
initWithName_controllerClass_delegateClassSelector :: Selector
initWithName_controllerClass_delegateClassSelector = mkSelector "initWithName:controllerClass:delegateClass:"

-- | @Selector@ for @bundle@
bundleSelector :: Selector
bundleSelector = mkSelector "bundle"

-- | @Selector@ for @paletteWillTerminate@
paletteWillTerminateSelector :: Selector
paletteWillTerminateSelector = mkSelector "paletteWillTerminate"

-- | @Selector@ for @lastKeyEventWasDeadKey@
lastKeyEventWasDeadKeySelector :: Selector
lastKeyEventWasDeadKeySelector = mkSelector "lastKeyEventWasDeadKey"

