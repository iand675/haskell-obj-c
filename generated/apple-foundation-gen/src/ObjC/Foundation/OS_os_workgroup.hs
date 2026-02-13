{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | os_workgroup_t
--
-- A reference counted os object representing a workload that needs to be distinctly recognized and tracked by the system.  The workgroup tracks a collection of threads all working cooperatively. An os_workgroup object - when not an instance of a specific os_workgroup_t subclass - represents a generic workload and makes no assumptions about the kind of work done.
--
-- Threads can explicitly join an os_workgroup_t to mark themselves as participants in the workload.
--
-- Generated bindings for @OS_os_workgroup@.
module ObjC.Foundation.OS_os_workgroup
  ( OS_os_workgroup
  , IsOS_os_workgroup(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

