import Wrapper and use the 'callAPI' supplying a 'ReqData' which atm needs to be
constructad by hand. See Abs.hs for information about the ReqData type.

To use the Haskell Eve API you can either import the predefined modules for the
services you want.

If the service you want to use is not available through the API or you want to
parse the data yourself you can use the Parseable class to define parseFun. The
Parseable class requires the language pragmas 'MultiParamTypeClasses' and
'FlexibleInstances'.

If your data type is of the type Constructable you can use the function stdParse
as the parser, which works for most services.

implemented services so far:
Account - All