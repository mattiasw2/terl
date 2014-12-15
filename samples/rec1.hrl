-record(contact, {
          name = "name_default" :: string(),
          age = -1 :: integer()
         }).

-record(contacts, {
          company = "company_default" :: string(),
          contacts = [] :: [#contact{}]
       }).


-record(device, {
          name = "name_default" :: string(),
          ip_address = "0.0.0.0" :: string()
         }).

-type mail() :: #contact{} | #device{}.
