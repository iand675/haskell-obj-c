{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPositionalSpecifier@.
module ObjC.Foundation.NSPositionalSpecifier
  ( NSPositionalSpecifier
  , IsNSPositionalSpecifier(..)
  , initWithPosition_objectSpecifier
  , setInsertionClassDescription
  , evaluate
  , position
  , insertionContainer
  , insertionKey
  , insertionIndex
  , insertionReplaces
  , initWithPosition_objectSpecifierSelector
  , setInsertionClassDescriptionSelector
  , evaluateSelector
  , positionSelector
  , insertionContainerSelector
  , insertionKeySelector
  , insertionIndexSelector
  , insertionReplacesSelector

  -- * Enum types
  , NSInsertionPosition(NSInsertionPosition)
  , pattern NSPositionAfter
  , pattern NSPositionBefore
  , pattern NSPositionBeginning
  , pattern NSPositionEnd
  , pattern NSPositionReplace

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

import ObjC.Foundation.Internal.Classes
import ObjC.CoreFoundation.Internal.Enums

-- | @- initWithPosition:objectSpecifier:@
initWithPosition_objectSpecifier :: (IsNSPositionalSpecifier nsPositionalSpecifier, IsNSScriptObjectSpecifier specifier) => nsPositionalSpecifier -> NSInsertionPosition -> specifier -> IO (Id NSPositionalSpecifier)
initWithPosition_objectSpecifier nsPositionalSpecifier  position specifier =
withObjCPtr specifier $ \raw_specifier ->
    sendMsg nsPositionalSpecifier (mkSelector "initWithPosition:objectSpecifier:") (retPtr retVoid) [argCULong (coerce position), argPtr (castPtr raw_specifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- setInsertionClassDescription:@
setInsertionClassDescription :: (IsNSPositionalSpecifier nsPositionalSpecifier, IsNSScriptClassDescription classDescription) => nsPositionalSpecifier -> classDescription -> IO ()
setInsertionClassDescription nsPositionalSpecifier  classDescription =
withObjCPtr classDescription $ \raw_classDescription ->
    sendMsg nsPositionalSpecifier (mkSelector "setInsertionClassDescription:") retVoid [argPtr (castPtr raw_classDescription :: Ptr ())]

-- | @- evaluate@
evaluate :: IsNSPositionalSpecifier nsPositionalSpecifier => nsPositionalSpecifier -> IO ()
evaluate nsPositionalSpecifier  =
  sendMsg nsPositionalSpecifier (mkSelector "evaluate") retVoid []

-- | @- position@
position :: IsNSPositionalSpecifier nsPositionalSpecifier => nsPositionalSpecifier -> IO NSInsertionPosition
position nsPositionalSpecifier  =
  fmap (coerce :: CULong -> NSInsertionPosition) $ sendMsg nsPositionalSpecifier (mkSelector "position") retCULong []

-- | @- insertionContainer@
insertionContainer :: IsNSPositionalSpecifier nsPositionalSpecifier => nsPositionalSpecifier -> IO RawId
insertionContainer nsPositionalSpecifier  =
  fmap (RawId . castPtr) $ sendMsg nsPositionalSpecifier (mkSelector "insertionContainer") (retPtr retVoid) []

-- | @- insertionKey@
insertionKey :: IsNSPositionalSpecifier nsPositionalSpecifier => nsPositionalSpecifier -> IO (Id NSString)
insertionKey nsPositionalSpecifier  =
  sendMsg nsPositionalSpecifier (mkSelector "insertionKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- insertionIndex@
insertionIndex :: IsNSPositionalSpecifier nsPositionalSpecifier => nsPositionalSpecifier -> IO CLong
insertionIndex nsPositionalSpecifier  =
  sendMsg nsPositionalSpecifier (mkSelector "insertionIndex") retCLong []

-- | @- insertionReplaces@
insertionReplaces :: IsNSPositionalSpecifier nsPositionalSpecifier => nsPositionalSpecifier -> IO Bool
insertionReplaces nsPositionalSpecifier  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPositionalSpecifier (mkSelector "insertionReplaces") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPosition:objectSpecifier:@
initWithPosition_objectSpecifierSelector :: Selector
initWithPosition_objectSpecifierSelector = mkSelector "initWithPosition:objectSpecifier:"

-- | @Selector@ for @setInsertionClassDescription:@
setInsertionClassDescriptionSelector :: Selector
setInsertionClassDescriptionSelector = mkSelector "setInsertionClassDescription:"

-- | @Selector@ for @evaluate@
evaluateSelector :: Selector
evaluateSelector = mkSelector "evaluate"

-- | @Selector@ for @position@
positionSelector :: Selector
positionSelector = mkSelector "position"

-- | @Selector@ for @insertionContainer@
insertionContainerSelector :: Selector
insertionContainerSelector = mkSelector "insertionContainer"

-- | @Selector@ for @insertionKey@
insertionKeySelector :: Selector
insertionKeySelector = mkSelector "insertionKey"

-- | @Selector@ for @insertionIndex@
insertionIndexSelector :: Selector
insertionIndexSelector = mkSelector "insertionIndex"

-- | @Selector@ for @insertionReplaces@
insertionReplacesSelector :: Selector
insertionReplacesSelector = mkSelector "insertionReplaces"

