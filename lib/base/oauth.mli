open Spotlib.Spot

type signature_method = [ `Hmac_sha1
                        | `Plaintext
                        | `Rsa_sha1 of Cryptokit.RSA.key 
                        ]

val fetch_request_token : 
  ?post:bool
  -> ?handle_tweak:(Curl.handle -> unit)
  -> host:string 
  -> ?port:int
  -> path:string 

  -> ?oauth_version:string 
  -> ?oauth_signature_method:signature_method 
  -> ?oauth_timestamp:float 
  -> ?oauth_nonce:string 
  -> ?oauth_other_params:(string * string) list
  -> oauth_consumer_key:string 
  -> oauth_consumer_secret:string 

  -> unit
  -> (string, [> Http.error]) Result.t 

val fetch_access_token : 
  verif:string 
  -> oauth_token:string 
  -> oauth_token_secret:string 
  -> ?post:bool
  -> ?handle_tweak:(Curl.handle -> unit)

  -> host:string 
  -> ?port:int
  -> path:string 

  -> ?oauth_version:string 
  -> ?oauth_signature_method:signature_method 
  -> ?oauth_timestamp:float 
  -> ?oauth_nonce:string 
  -> oauth_consumer_key:string 
  -> oauth_consumer_secret:string 

  -> unit
  -> (string, [> Http.error]) Result.t 

type t = {
  consumer_key        : string;
  consumer_secret     : string;
  access_token        : string;
  access_token_secret : string;
} with conv(ocaml)

val access :
  [ `HTTP | `HTTPS ]
  -> ?oauth_other_params: Http.params
  -> t 
  -> string (** host *)
  -> string (** path *) 
  -> meth:[< `GET of Http.params
          | `POST of Http.params
          | `POST2 of (string * [ `CONTENT of string | `FILE of string ]) list ]
  -> (string, [> Http.error]) Result.t 
