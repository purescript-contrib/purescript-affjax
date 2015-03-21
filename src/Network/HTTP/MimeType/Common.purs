module Network.HTTP.MimeType.Common where

import Network.HTTP.MimeType

applicationFormURLEncoded :: MimeType
applicationFormURLEncoded = MimeType "application/x-www-form-urlencoded"

applicationJSON :: MimeType
applicationJSON = MimeType "application/json"

applicationJavascript :: MimeType
applicationJavascript = MimeType "application/javascript"

applicationOctetStream :: MimeType
applicationOctetStream = MimeType "application/octet-stream"

applicationXML :: MimeType
applicationXML = MimeType "application/xml"

imageGIF :: MimeType
imageGIF = MimeType "image/gif"

imageJPEG :: MimeType
imageJPEG = MimeType "image/jpeg"

imagePNG :: MimeType
imagePNG = MimeType "image/png"

multipartFormData :: MimeType
multipartFormData = MimeType "multipart/form-data"

textCSV :: MimeType
textCSV = MimeType "text/csv"

textHTML :: MimeType
textHTML = MimeType "text/html"

textPlain :: MimeType
textPlain = MimeType "text/plain"

textXML :: MimeType
textXML = MimeType "text/xml"
