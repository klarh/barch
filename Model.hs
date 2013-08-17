module Model where

import Prelude
import Yesod
import Yesod.Markdown
import Data.Text (Text)
import Data.Map
import Database.Persist.Quasi
import Database.Persist.MongoDB hiding (master)
import Language.Haskell.TH.Syntax
import Data.Typeable (Typeable)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
let mongoSettings = (mkPersistSettings (ConT ''MongoBackend))
                        { mpsGeneric = False
                        }
 in share [mkPersist mongoSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
