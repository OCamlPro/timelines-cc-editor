module Arg = struct
  type user_id = int32
  type user_info = SpotTypes.User.user_info
  let user_id_encoding = Json_encoding.int32
  let user_info_encoding = SpotAPIEncoding.UserEncoding.user_info_encoding
  let rpc_path = []
  let token_kind = `CSRF "X-Csrf-Token" (* `Cookie "EZSESSION" *)
end
