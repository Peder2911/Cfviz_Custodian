docker run -v "/home/peder/Projects/Custodian/data":/var/data -v "/home/peder/Projects/Custodian/config":/var/config --link gsql:gsql --name aCustodian --rm custodian
