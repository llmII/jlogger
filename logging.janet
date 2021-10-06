# use file/open path "w+n" (error when the file can't be opened, create if not exist)
# user is responsible for directories, do not create these for them

(import logging/sinks :as sinks)
# export logging/formatters as logging/formatters/$symbol
(import logging/formatters :as formatters :export true)
# export logging/namers as logging/namers/$symbol
(import logging/namers :as namers :export true)
# export logging/registry as logging/registry
(import logging/registry :as registry :export true)
# get our registry of sinks/formatters/namers!
(import logging/registry-internal :as reg)
# import log-manager
(import logging/manager :as manager)
(use logging/misc)

# export manager
# TODO: depend on llmII/jumble and defalias to pull in the documentation...
(def manager manager/log-manager)
