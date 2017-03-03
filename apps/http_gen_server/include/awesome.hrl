-define(record_to_tuplelist(Rec, Ref), lists:zip(record_info(fields, Rec),tl(tuple_to_list(Ref)))).
-define(APP_BLICUP,0).
-define(SEARCH_SIZE,<<"40">>).
-define(SCROLLING_SIZE,<<"30">>).
-define(USER_STATUS_ACTIVE, 1).
-define(USER_STATUS_BLOCKED, -1).
-record(user, {
          userId :: binary(),
	  twitterId :: binary(),
	  facebookId :: binary(),
	  signupIp :: binary(),
          username :: binary(),
          bio  :: binary(),
	  photoUrl :: binary(),
          signupDate :: integer(),
	  mainLocale :: binary(),
          likeCount :: integer(),
          featuredCount :: integer(),
	  followerCount :: integer(),
	  followeeCount :: integer(),
	  viewsCount :: integer(),
	  isVerified :: integer(),
	  isAdm = 0 :: integer(),
	  status = 1 :: integer()
	 }).

-record(video, {
          videoId :: binary(),
          videoDescription :: binary(),
	  tags :: list(binary()),
	  mentionList :: list(binary()),
	  photoUrl :: binary(),
	  thumbGifUrl :: binary(),
	  lowQualityUrl :: binary(),
	  bestQualityUrl :: binary(),
	  filterSpec :: integer(),
	  author,
	  creationDate :: integer(),
	  featuredDate :: integer(),
	  likedDate :: integer(),
	  updateDate :: integer(),
          likeCount :: integer(),
	  viewsCount :: integer(),
	  commentsCount :: integer(),
	  categoryId :: binary(),
	  isFeatured :: integer(),
	  deleteDate :: integer()
         }).


-record(user_followee, {
	  userId_followeeId :: binary(),
          userId :: binary(),
          followeeId :: binary()
	 }).

-record(user_follower, {
	  userId_followerId :: binary(),
          userId :: binary(),
          followerId :: binary()
	 }).


-record(video_like, {
          videoId_userId :: binary(),
          videoId :: binary(),
          userId :: binary(),
	  likeDate :: integer()
         }).


-export([get_full_key/1]).

get_full_key(Video) when is_record(Video,video) ->
    [{<<"videoId">>,Video#video.videoId},{<<"creationDate">>, Video#video.creationDate}];

get_full_key(VideoLike) when is_record(VideoLike,video_like) ->
    [{<<"videoId_userId">>,VideoLike#video_like.videoId_userId},{<<"userId">>, VideoLike#video_like.userId}];

get_full_key(UserFollowee) when is_record(UserFollowee,user_followee) ->
    [{<<"userId_followeeId">>,UserFollowee#user_followee.userId_followeeId},{<<"userId">>, UserFollowee#user_followee.userId}];

get_full_key(UserFollower) when is_record(UserFollower,user_follower) ->
    [{<<"userId_followerId">>,UserFollower#user_follower.userId_followerId},{<<"userId">>, UserFollower#user_follower.userId}];

get_full_key(User) when is_record(User,user) ->
    [{<<"userId">>,User#user.userId},{<<"signupDate">>, User#user.signupDate}].


