#' @title Twitter-data retrieval (full)
#' @description This function grabs as much data as it can from Twitter. This is the full version of the function which parses tweets one by one to extract as much info as possible, including hashtags and image links. Column "origins" contains details about where the tweet comes from (i.e. retweet from other users, in chronological order). 
#' @param username The name of the twitter follower you wish to lookup.
#' @param pages The number of tweet pages you want to retrieve. About 20 tweets per page.
#' @return Returns a tibble containing tweet-data for the specified used.
#' @examples
#' get_tweets_full(username = 'hadleywickham', pages = 5)
get_tweets_full = function(username, pages = 1) {

  suppressMessages({
    require(httr)
    require(rvest)
    require(dplyr)
    require(magrittr)
    require(tidyr)
    require(tibble)
    require(xml2)
  })

  org_pages = pages

  # To store the results.
  dataset = list()

  # Url to target.
  url = stringr::str_interp('https://twitter.com/i/profiles/show/${username}/timeline/tweets?include_available_features=1&include_entities=1&include_new_items_bar=true')

  # Initial tweet collection (latest tweets)
  get = GET(
    url = url,
    add_headers(
      "Accept" = 'application/json, text/javascript, */*; q=0.01',
      "Referer" = stringr::str_interp('https://twitter.com/${username}'),
      "User-Agent" = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/603.3.8 (KHTML, like Gecko) Version/10.1.2 Safari/603.3.8',
      "X-Twitter-Active-User" = 'yes',
      "X-Requested-With" = 'XMLHttpRequest'
    )
  ) %>%
    extract2("content") %>%
    rawToChar() %>%
    jsonlite::fromJSON()
  
  # Extract list of tweets (about 20)
  tweets = get %>%
    extract2("items_html") %>%
    read_html()

  while (T) {

    if(pages == 0) break

    storage = list()
    for (i in 1:length(rvest::html_nodes(tweets, ".stream-item"))) {

      tweet = rvest::html_nodes(tweets, ".stream-item")[[i]]

      tweet_text = tweet %>%
        rvest::html_nodes(".js-tweet-text") %>%
        rvest::html_text() %>%
        ifelse(length(.) == 0, NA, .)

      tweet_id = tweet %>%
        rvest::html_nodes(".tweet-timestamp") %>%
        rvest::html_attr("data-conversation-id") %>%
        ifelse(length(.) == 0, NA, .) %>%
        as.numeric()

      tweet_timestamp = tweet %>%
        rvest::html_nodes("._timestamp") %>%
        rvest::html_attr("data-time") %>%
        as.numeric() %>%
        as.POSIXct(origin = "1970-01-01") %>%
        ifelse(length(.) == 0, NA, .)

      tweet_interactions = tweet %>%
        rvest::html_nodes(".ProfileTweet-actionCountForAria") %>%
        rvest::html_text()

      # If one exists, all exist. If none exist, set all to 0.
      if (length(tweet_interactions) == 0){
        tweet_interactions = tibble(
          likes = 0,
          retweets = 0,
          replies = 0
        )
      } else {
        tweet_interactions %<>%
          as.tibble() %>%
          mutate(type = rep(c("replies", "retweets", "likes"), length(tweet_id)),
                 value = stringr::str_replace_all(value, "\\D", "") %>%
                   as.numeric(),
                 rn = row_number()) %>%
          spread(type, value)
      }

      tweet_likes = tweet_interactions$likes %>%
        na.omit() %>%
        as.vector() %>%
        ifelse(length(.) == 0, NA, .)

      tweet_retweets = tweet_interactions$retweets %>%
        na.omit() %>%
        as.vector() %>%
        ifelse(length(.) == 0, NA, .)

      tweet_replies = tweet_interactions$replies %>%
        na.omit() %>%
        as.vector() %>%
        ifelse(length(.) == 0, NA, .)

      tweet_hashtags = tweet %>%
        rvest::html_nodes(".twitter-hashtag") %>%
        rvest::html_text() %>%
        ifelse(length(.) == 0, NA, .)

      urls = tweet %>%
        rvest::html_nodes("a.twitter-timeline-link:not(.u-hidden)") %>%
        rvest::html_attr("data-expanded-url") %>%
        ifelse(length(.) == 0, NA, .)

      photos = tweet %>%
        rvest::html_nodes(".AdaptiveMedia-photoContainer") %>%
        rvest::html_attr("data-image-url") %>%
        ifelse(length(.) == 0, NA, .)

      tweet_reply_json = tweet %>%
        rvest::html_nodes(".js-stream-tweet") %>%
        rvest::html_attr("data-reply-to-users-json")

      if(length(tweet_reply_json) != 0) {
        tweet_reply_json = tweet %>%
          rvest::html_nodes(".js-stream-tweet") %>%
          rvest::html_attr("data-reply-to-users-json") %>%
          jsonlite::fromJSON(flatten = T) %>%
          filter(screen_name != username) %>%
          select(origin_usr_id = id_str,
                 origin_usr_screen_name = screen_name,
                 origin_usr_name = name,
                 origin_usr_emojified_name = emojified_name.text)

      } else {
        tweet_reply_json = tibble(origin_usr_id = NA,
                                  origin_usr_screen_name = NA,
                                  origin_usr_name = NA,
                                  origin_usr_emojified_name = NA)
      }

      if(nrow(tweet_reply_json) == 0)
        tweet_reply_json[nrow(tweet_reply_json)+1, ] = NA

      storage[[i]] = tibble(
          id = tweet_id,
          timestamp = tweet_timestamp,
          text = tweet_text,
          likes = tweet_likes,
          retweets = tweet_retweets,
          replies = tweet_replies,
          urls = urls,
          photos = photos
        ) %>%
        cbind(tweet_reply_json) %>%
        mutate(
          timestamp = as.POSIXct(timestamp, origin = "1970-01-01")
        ) %>%
        nest(origin_usr_id,
             origin_usr_screen_name,
             origin_usr_name,
             origin_usr_emojified_name) %>% 
        rename(origins = data)

    } #for

    dataset[[pages]] = storage %>%
      data.table::rbindlist() %>%
      as.tibble()

    # For next iteration, sets "max id" to get.
    min_id = get$min_position

    paste0("Finished page ", org_pages-pages+1, ". Proceeding to next.") %>%
      print()

    # Get next tweets (start from min of previous page).
    get = GET(
      url = url,
      add_headers(
        "Accept" = 'application/json, text/javascript, */*; q=0.01',
        "Referer" = stringr::str_interp('https://twitter.com/${username}'),
        "User-Agent" = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/603.3.8 (KHTML, like Gecko) Version/10.1.2 Safari/603.3.8',
        "X-Twitter-Active-User" = 'yes',
        "X-Requested-With" = 'XMLHttpRequest'
      ),
      query = list(max_position = min_id)
    ) %>%
      extract2("content") %>%
      rawToChar() %>%
      jsonlite::fromJSON()
    
    # If there's nothing more to return (usually if we hit the 40 page limit).
    if(get$has_more_items){
      tweets = get %>%
        extract2("items_html") %>%
        read_html()
    } else {
      warning(paste("Stopped before last page. Unable to retrieve more tweets for user", username, "from page", org_pages-pages+1))
      break
    }

    pages = pages - 1

  } #while

  return(dataset %>%
           data.table::rbindlist() %>%
           as.tibble())

}
