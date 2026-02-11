{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPredicateEditorRowTemplate@.
module ObjC.AppKit.NSPredicateEditorRowTemplate
  ( NSPredicateEditorRowTemplate
  , IsNSPredicateEditorRowTemplate(..)
  , matchForPredicate
  , setPredicate
  , predicateWithSubpredicates
  , displayableSubpredicatesOfPredicate
  , initWithLeftExpressions_rightExpressions_modifier_operators_options
  , initWithLeftExpressions_rightExpressionAttributeType_modifier_operators_options
  , initWithCompoundTypes
  , templatesWithAttributeKeyPaths_inEntityDescription
  , templateViews
  , leftExpressions
  , rightExpressions
  , rightExpressionAttributeType
  , modifier
  , operators
  , options
  , compoundTypes
  , matchForPredicateSelector
  , setPredicateSelector
  , predicateWithSubpredicatesSelector
  , displayableSubpredicatesOfPredicateSelector
  , initWithLeftExpressions_rightExpressions_modifier_operators_optionsSelector
  , initWithLeftExpressions_rightExpressionAttributeType_modifier_operators_optionsSelector
  , initWithCompoundTypesSelector
  , templatesWithAttributeKeyPaths_inEntityDescriptionSelector
  , templateViewsSelector
  , leftExpressionsSelector
  , rightExpressionsSelector
  , rightExpressionAttributeTypeSelector
  , modifierSelector
  , operatorsSelector
  , optionsSelector
  , compoundTypesSelector

  -- * Enum types
  , NSAttributeType(NSAttributeType)
  , pattern NSUndefinedAttributeType
  , pattern NSInteger16AttributeType
  , pattern NSInteger32AttributeType
  , pattern NSInteger64AttributeType
  , pattern NSDecimalAttributeType
  , pattern NSDoubleAttributeType
  , pattern NSFloatAttributeType
  , pattern NSStringAttributeType
  , pattern NSBooleanAttributeType
  , pattern NSDateAttributeType
  , pattern NSBinaryDataAttributeType
  , pattern NSUUIDAttributeType
  , pattern NSURIAttributeType
  , pattern NSTransformableAttributeType
  , pattern NSObjectIDAttributeType
  , pattern NSCompositeAttributeType
  , NSComparisonPredicateModifier(NSComparisonPredicateModifier)
  , pattern NSDirectPredicateModifier
  , pattern NSAllPredicateModifier
  , pattern NSAnyPredicateModifier

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

import ObjC.AppKit.Internal.Classes
import ObjC.CoreData.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- matchForPredicate:@
matchForPredicate :: (IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate, IsNSPredicate predicate) => nsPredicateEditorRowTemplate -> predicate -> IO CDouble
matchForPredicate nsPredicateEditorRowTemplate  predicate =
withObjCPtr predicate $ \raw_predicate ->
    sendMsg nsPredicateEditorRowTemplate (mkSelector "matchForPredicate:") retCDouble [argPtr (castPtr raw_predicate :: Ptr ())]

-- | @- setPredicate:@
setPredicate :: (IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate, IsNSPredicate predicate) => nsPredicateEditorRowTemplate -> predicate -> IO ()
setPredicate nsPredicateEditorRowTemplate  predicate =
withObjCPtr predicate $ \raw_predicate ->
    sendMsg nsPredicateEditorRowTemplate (mkSelector "setPredicate:") retVoid [argPtr (castPtr raw_predicate :: Ptr ())]

-- | @- predicateWithSubpredicates:@
predicateWithSubpredicates :: (IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate, IsNSArray subpredicates) => nsPredicateEditorRowTemplate -> subpredicates -> IO (Id NSPredicate)
predicateWithSubpredicates nsPredicateEditorRowTemplate  subpredicates =
withObjCPtr subpredicates $ \raw_subpredicates ->
    sendMsg nsPredicateEditorRowTemplate (mkSelector "predicateWithSubpredicates:") (retPtr retVoid) [argPtr (castPtr raw_subpredicates :: Ptr ())] >>= retainedObject . castPtr

-- | @- displayableSubpredicatesOfPredicate:@
displayableSubpredicatesOfPredicate :: (IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate, IsNSPredicate predicate) => nsPredicateEditorRowTemplate -> predicate -> IO (Id NSArray)
displayableSubpredicatesOfPredicate nsPredicateEditorRowTemplate  predicate =
withObjCPtr predicate $ \raw_predicate ->
    sendMsg nsPredicateEditorRowTemplate (mkSelector "displayableSubpredicatesOfPredicate:") (retPtr retVoid) [argPtr (castPtr raw_predicate :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithLeftExpressions:rightExpressions:modifier:operators:options:@
initWithLeftExpressions_rightExpressions_modifier_operators_options :: (IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate, IsNSArray leftExpressions, IsNSArray rightExpressions, IsNSArray operators) => nsPredicateEditorRowTemplate -> leftExpressions -> rightExpressions -> NSComparisonPredicateModifier -> operators -> CULong -> IO (Id NSPredicateEditorRowTemplate)
initWithLeftExpressions_rightExpressions_modifier_operators_options nsPredicateEditorRowTemplate  leftExpressions rightExpressions modifier operators options =
withObjCPtr leftExpressions $ \raw_leftExpressions ->
  withObjCPtr rightExpressions $ \raw_rightExpressions ->
    withObjCPtr operators $ \raw_operators ->
        sendMsg nsPredicateEditorRowTemplate (mkSelector "initWithLeftExpressions:rightExpressions:modifier:operators:options:") (retPtr retVoid) [argPtr (castPtr raw_leftExpressions :: Ptr ()), argPtr (castPtr raw_rightExpressions :: Ptr ()), argCULong (coerce modifier), argPtr (castPtr raw_operators :: Ptr ()), argCULong (fromIntegral options)] >>= ownedObject . castPtr

-- | @- initWithLeftExpressions:rightExpressionAttributeType:modifier:operators:options:@
initWithLeftExpressions_rightExpressionAttributeType_modifier_operators_options :: (IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate, IsNSArray leftExpressions, IsNSArray operators) => nsPredicateEditorRowTemplate -> leftExpressions -> NSAttributeType -> NSComparisonPredicateModifier -> operators -> CULong -> IO (Id NSPredicateEditorRowTemplate)
initWithLeftExpressions_rightExpressionAttributeType_modifier_operators_options nsPredicateEditorRowTemplate  leftExpressions attributeType modifier operators options =
withObjCPtr leftExpressions $ \raw_leftExpressions ->
  withObjCPtr operators $ \raw_operators ->
      sendMsg nsPredicateEditorRowTemplate (mkSelector "initWithLeftExpressions:rightExpressionAttributeType:modifier:operators:options:") (retPtr retVoid) [argPtr (castPtr raw_leftExpressions :: Ptr ()), argCULong (coerce attributeType), argCULong (coerce modifier), argPtr (castPtr raw_operators :: Ptr ()), argCULong (fromIntegral options)] >>= ownedObject . castPtr

-- | @- initWithCompoundTypes:@
initWithCompoundTypes :: (IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate, IsNSArray compoundTypes) => nsPredicateEditorRowTemplate -> compoundTypes -> IO (Id NSPredicateEditorRowTemplate)
initWithCompoundTypes nsPredicateEditorRowTemplate  compoundTypes =
withObjCPtr compoundTypes $ \raw_compoundTypes ->
    sendMsg nsPredicateEditorRowTemplate (mkSelector "initWithCompoundTypes:") (retPtr retVoid) [argPtr (castPtr raw_compoundTypes :: Ptr ())] >>= ownedObject . castPtr

-- | @+ templatesWithAttributeKeyPaths:inEntityDescription:@
templatesWithAttributeKeyPaths_inEntityDescription :: (IsNSArray keyPaths, IsNSEntityDescription entityDescription) => keyPaths -> entityDescription -> IO (Id NSArray)
templatesWithAttributeKeyPaths_inEntityDescription keyPaths entityDescription =
  do
    cls' <- getRequiredClass "NSPredicateEditorRowTemplate"
    withObjCPtr keyPaths $ \raw_keyPaths ->
      withObjCPtr entityDescription $ \raw_entityDescription ->
        sendClassMsg cls' (mkSelector "templatesWithAttributeKeyPaths:inEntityDescription:") (retPtr retVoid) [argPtr (castPtr raw_keyPaths :: Ptr ()), argPtr (castPtr raw_entityDescription :: Ptr ())] >>= retainedObject . castPtr

-- | @- templateViews@
templateViews :: IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate => nsPredicateEditorRowTemplate -> IO (Id NSArray)
templateViews nsPredicateEditorRowTemplate  =
  sendMsg nsPredicateEditorRowTemplate (mkSelector "templateViews") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- leftExpressions@
leftExpressions :: IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate => nsPredicateEditorRowTemplate -> IO (Id NSArray)
leftExpressions nsPredicateEditorRowTemplate  =
  sendMsg nsPredicateEditorRowTemplate (mkSelector "leftExpressions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rightExpressions@
rightExpressions :: IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate => nsPredicateEditorRowTemplate -> IO (Id NSArray)
rightExpressions nsPredicateEditorRowTemplate  =
  sendMsg nsPredicateEditorRowTemplate (mkSelector "rightExpressions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rightExpressionAttributeType@
rightExpressionAttributeType :: IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate => nsPredicateEditorRowTemplate -> IO NSAttributeType
rightExpressionAttributeType nsPredicateEditorRowTemplate  =
  fmap (coerce :: CULong -> NSAttributeType) $ sendMsg nsPredicateEditorRowTemplate (mkSelector "rightExpressionAttributeType") retCULong []

-- | @- modifier@
modifier :: IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate => nsPredicateEditorRowTemplate -> IO NSComparisonPredicateModifier
modifier nsPredicateEditorRowTemplate  =
  fmap (coerce :: CULong -> NSComparisonPredicateModifier) $ sendMsg nsPredicateEditorRowTemplate (mkSelector "modifier") retCULong []

-- | @- operators@
operators :: IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate => nsPredicateEditorRowTemplate -> IO (Id NSArray)
operators nsPredicateEditorRowTemplate  =
  sendMsg nsPredicateEditorRowTemplate (mkSelector "operators") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- options@
options :: IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate => nsPredicateEditorRowTemplate -> IO CULong
options nsPredicateEditorRowTemplate  =
  sendMsg nsPredicateEditorRowTemplate (mkSelector "options") retCULong []

-- | @- compoundTypes@
compoundTypes :: IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate => nsPredicateEditorRowTemplate -> IO (Id NSArray)
compoundTypes nsPredicateEditorRowTemplate  =
  sendMsg nsPredicateEditorRowTemplate (mkSelector "compoundTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @matchForPredicate:@
matchForPredicateSelector :: Selector
matchForPredicateSelector = mkSelector "matchForPredicate:"

-- | @Selector@ for @setPredicate:@
setPredicateSelector :: Selector
setPredicateSelector = mkSelector "setPredicate:"

-- | @Selector@ for @predicateWithSubpredicates:@
predicateWithSubpredicatesSelector :: Selector
predicateWithSubpredicatesSelector = mkSelector "predicateWithSubpredicates:"

-- | @Selector@ for @displayableSubpredicatesOfPredicate:@
displayableSubpredicatesOfPredicateSelector :: Selector
displayableSubpredicatesOfPredicateSelector = mkSelector "displayableSubpredicatesOfPredicate:"

-- | @Selector@ for @initWithLeftExpressions:rightExpressions:modifier:operators:options:@
initWithLeftExpressions_rightExpressions_modifier_operators_optionsSelector :: Selector
initWithLeftExpressions_rightExpressions_modifier_operators_optionsSelector = mkSelector "initWithLeftExpressions:rightExpressions:modifier:operators:options:"

-- | @Selector@ for @initWithLeftExpressions:rightExpressionAttributeType:modifier:operators:options:@
initWithLeftExpressions_rightExpressionAttributeType_modifier_operators_optionsSelector :: Selector
initWithLeftExpressions_rightExpressionAttributeType_modifier_operators_optionsSelector = mkSelector "initWithLeftExpressions:rightExpressionAttributeType:modifier:operators:options:"

-- | @Selector@ for @initWithCompoundTypes:@
initWithCompoundTypesSelector :: Selector
initWithCompoundTypesSelector = mkSelector "initWithCompoundTypes:"

-- | @Selector@ for @templatesWithAttributeKeyPaths:inEntityDescription:@
templatesWithAttributeKeyPaths_inEntityDescriptionSelector :: Selector
templatesWithAttributeKeyPaths_inEntityDescriptionSelector = mkSelector "templatesWithAttributeKeyPaths:inEntityDescription:"

-- | @Selector@ for @templateViews@
templateViewsSelector :: Selector
templateViewsSelector = mkSelector "templateViews"

-- | @Selector@ for @leftExpressions@
leftExpressionsSelector :: Selector
leftExpressionsSelector = mkSelector "leftExpressions"

-- | @Selector@ for @rightExpressions@
rightExpressionsSelector :: Selector
rightExpressionsSelector = mkSelector "rightExpressions"

-- | @Selector@ for @rightExpressionAttributeType@
rightExpressionAttributeTypeSelector :: Selector
rightExpressionAttributeTypeSelector = mkSelector "rightExpressionAttributeType"

-- | @Selector@ for @modifier@
modifierSelector :: Selector
modifierSelector = mkSelector "modifier"

-- | @Selector@ for @operators@
operatorsSelector :: Selector
operatorsSelector = mkSelector "operators"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @compoundTypes@
compoundTypesSelector :: Selector
compoundTypesSelector = mkSelector "compoundTypes"

