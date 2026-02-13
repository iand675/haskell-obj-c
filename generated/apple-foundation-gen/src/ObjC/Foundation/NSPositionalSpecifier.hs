{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , objectSpecifier
  , insertionContainer
  , insertionKey
  , insertionIndex
  , insertionReplaces
  , evaluateSelector
  , initWithPosition_objectSpecifierSelector
  , insertionContainerSelector
  , insertionIndexSelector
  , insertionKeySelector
  , insertionReplacesSelector
  , objectSpecifierSelector
  , positionSelector
  , setInsertionClassDescriptionSelector

  -- * Enum types
  , NSInsertionPosition(NSInsertionPosition)
  , pattern NSPositionAfter
  , pattern NSPositionBefore
  , pattern NSPositionBeginning
  , pattern NSPositionEnd
  , pattern NSPositionReplace

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- initWithPosition:objectSpecifier:@
initWithPosition_objectSpecifier :: (IsNSPositionalSpecifier nsPositionalSpecifier, IsNSScriptObjectSpecifier specifier) => nsPositionalSpecifier -> NSInsertionPosition -> specifier -> IO (Id NSPositionalSpecifier)
initWithPosition_objectSpecifier nsPositionalSpecifier position specifier =
  sendOwnedMessage nsPositionalSpecifier initWithPosition_objectSpecifierSelector position (toNSScriptObjectSpecifier specifier)

-- | @- setInsertionClassDescription:@
setInsertionClassDescription :: (IsNSPositionalSpecifier nsPositionalSpecifier, IsNSScriptClassDescription classDescription) => nsPositionalSpecifier -> classDescription -> IO ()
setInsertionClassDescription nsPositionalSpecifier classDescription =
  sendMessage nsPositionalSpecifier setInsertionClassDescriptionSelector (toNSScriptClassDescription classDescription)

-- | @- evaluate@
evaluate :: IsNSPositionalSpecifier nsPositionalSpecifier => nsPositionalSpecifier -> IO ()
evaluate nsPositionalSpecifier =
  sendMessage nsPositionalSpecifier evaluateSelector

-- | @- position@
position :: IsNSPositionalSpecifier nsPositionalSpecifier => nsPositionalSpecifier -> IO NSInsertionPosition
position nsPositionalSpecifier =
  sendMessage nsPositionalSpecifier positionSelector

-- | @- objectSpecifier@
objectSpecifier :: IsNSPositionalSpecifier nsPositionalSpecifier => nsPositionalSpecifier -> IO (Id NSScriptObjectSpecifier)
objectSpecifier nsPositionalSpecifier =
  sendMessage nsPositionalSpecifier objectSpecifierSelector

-- | @- insertionContainer@
insertionContainer :: IsNSPositionalSpecifier nsPositionalSpecifier => nsPositionalSpecifier -> IO RawId
insertionContainer nsPositionalSpecifier =
  sendMessage nsPositionalSpecifier insertionContainerSelector

-- | @- insertionKey@
insertionKey :: IsNSPositionalSpecifier nsPositionalSpecifier => nsPositionalSpecifier -> IO (Id NSString)
insertionKey nsPositionalSpecifier =
  sendMessage nsPositionalSpecifier insertionKeySelector

-- | @- insertionIndex@
insertionIndex :: IsNSPositionalSpecifier nsPositionalSpecifier => nsPositionalSpecifier -> IO CLong
insertionIndex nsPositionalSpecifier =
  sendMessage nsPositionalSpecifier insertionIndexSelector

-- | @- insertionReplaces@
insertionReplaces :: IsNSPositionalSpecifier nsPositionalSpecifier => nsPositionalSpecifier -> IO Bool
insertionReplaces nsPositionalSpecifier =
  sendMessage nsPositionalSpecifier insertionReplacesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPosition:objectSpecifier:@
initWithPosition_objectSpecifierSelector :: Selector '[NSInsertionPosition, Id NSScriptObjectSpecifier] (Id NSPositionalSpecifier)
initWithPosition_objectSpecifierSelector = mkSelector "initWithPosition:objectSpecifier:"

-- | @Selector@ for @setInsertionClassDescription:@
setInsertionClassDescriptionSelector :: Selector '[Id NSScriptClassDescription] ()
setInsertionClassDescriptionSelector = mkSelector "setInsertionClassDescription:"

-- | @Selector@ for @evaluate@
evaluateSelector :: Selector '[] ()
evaluateSelector = mkSelector "evaluate"

-- | @Selector@ for @position@
positionSelector :: Selector '[] NSInsertionPosition
positionSelector = mkSelector "position"

-- | @Selector@ for @objectSpecifier@
objectSpecifierSelector :: Selector '[] (Id NSScriptObjectSpecifier)
objectSpecifierSelector = mkSelector "objectSpecifier"

-- | @Selector@ for @insertionContainer@
insertionContainerSelector :: Selector '[] RawId
insertionContainerSelector = mkSelector "insertionContainer"

-- | @Selector@ for @insertionKey@
insertionKeySelector :: Selector '[] (Id NSString)
insertionKeySelector = mkSelector "insertionKey"

-- | @Selector@ for @insertionIndex@
insertionIndexSelector :: Selector '[] CLong
insertionIndexSelector = mkSelector "insertionIndex"

-- | @Selector@ for @insertionReplaces@
insertionReplacesSelector :: Selector '[] Bool
insertionReplacesSelector = mkSelector "insertionReplaces"

