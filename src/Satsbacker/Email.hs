
module Satsbacker.Email where

import Network.Mail.SMTP


from    = Address (Just "satsbacker test") "test@satsbacker.com"
to      = [Address (Just "William Casarin") "jb55@jb55.com"]
cc      = []
bcc     = []
subject = "satsbacker email test"
body    = plainTextPart "hello from satsbacker"
html    = htmlPart "<h1>Hello there</h1><p>Hello from satsbacker</p>"
host    = "satsbacker.com"

mail = simpleMail from to cc bcc subject [body, html]

main = renderSendMail mail



signupEmail = simpleMail from to cc bcc subject [body, html]
  where
    to = 
