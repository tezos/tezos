include Tezos_embedded_raw_protocol_alpha
module Environment = Tezos_embedded_protocol_environment_alpha.Environment
module Error_monad = Environment.Error_monad
type error = Error_monad.error
type 'a tzresult = 'a Error_monad.tzresult
