access_token <- "728299675489468416-kBJloyGVr1vHN1dKe5WATByEfyBm41q"
access_secret <- "U700dpsuJvvtRQ6VKgGcfCqfNDvJYP0p2nsBtVbnESrSW"

# whatever name you assigned to your created app
appname <- "CGU_SSPE_PROJECT"

## api key (example below is not a real key)
key <- "AUYlxd841CdMZEiwKmz8IRZmP"

## api secret (example below is not a real key)
secret <- "DPnZh2BAhKTJNnzLnLDZe05MFtu9ItewxEbfoaSUvdwzyials1"

setup_twitter_oauth(key, secret, access_token, access_secret)

# create token named "twitter_token"

SearchTerms <- "Something relevent"

recent_tweets <- searchTwitter(SearchTerms, n = 100, since = "2019-10-31", retryOnRateLimit=120)



past_tweets <- searchTwitter(SearchTerms, n = 100, since = "2007-10-09", until = "2008-4-30" retryOnRateLimit=120)
