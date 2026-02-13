{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterProgramCategoryStruct@.
module ObjC.Matter.MTRChannelClusterProgramCategoryStruct
  ( MTRChannelClusterProgramCategoryStruct
  , IsMTRChannelClusterProgramCategoryStruct(..)
  , category
  , setCategory
  , subCategory
  , setSubCategory
  , categorySelector
  , setCategorySelector
  , setSubCategorySelector
  , subCategorySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- category@
category :: IsMTRChannelClusterProgramCategoryStruct mtrChannelClusterProgramCategoryStruct => mtrChannelClusterProgramCategoryStruct -> IO (Id NSString)
category mtrChannelClusterProgramCategoryStruct =
  sendMessage mtrChannelClusterProgramCategoryStruct categorySelector

-- | @- setCategory:@
setCategory :: (IsMTRChannelClusterProgramCategoryStruct mtrChannelClusterProgramCategoryStruct, IsNSString value) => mtrChannelClusterProgramCategoryStruct -> value -> IO ()
setCategory mtrChannelClusterProgramCategoryStruct value =
  sendMessage mtrChannelClusterProgramCategoryStruct setCategorySelector (toNSString value)

-- | @- subCategory@
subCategory :: IsMTRChannelClusterProgramCategoryStruct mtrChannelClusterProgramCategoryStruct => mtrChannelClusterProgramCategoryStruct -> IO (Id NSString)
subCategory mtrChannelClusterProgramCategoryStruct =
  sendMessage mtrChannelClusterProgramCategoryStruct subCategorySelector

-- | @- setSubCategory:@
setSubCategory :: (IsMTRChannelClusterProgramCategoryStruct mtrChannelClusterProgramCategoryStruct, IsNSString value) => mtrChannelClusterProgramCategoryStruct -> value -> IO ()
setSubCategory mtrChannelClusterProgramCategoryStruct value =
  sendMessage mtrChannelClusterProgramCategoryStruct setSubCategorySelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @category@
categorySelector :: Selector '[] (Id NSString)
categorySelector = mkSelector "category"

-- | @Selector@ for @setCategory:@
setCategorySelector :: Selector '[Id NSString] ()
setCategorySelector = mkSelector "setCategory:"

-- | @Selector@ for @subCategory@
subCategorySelector :: Selector '[] (Id NSString)
subCategorySelector = mkSelector "subCategory"

-- | @Selector@ for @setSubCategory:@
setSubCategorySelector :: Selector '[Id NSString] ()
setSubCategorySelector = mkSelector "setSubCategory:"

