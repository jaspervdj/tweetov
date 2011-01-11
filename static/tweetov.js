/* Global variables to cache the user and tweet data. */
var user;
var tweet_data;

function submit_username() {
    var val = $('#usernamefield').val();

    /* We can use the cache and return. */
    if (user == val) {
        generate_tweet(tweet_data);
        return false;
    }

    user = val;
    $('#tweet').html('Getting tweets...');

    set_user(user);
     
    $.getJSON('http://api.twitter.com/1/statuses/user_timeline.json?screen_name=' + user + '&count=200&trim_user=1&callback=?',
        function(tweets) {
            minify_tweets(tweets);
            tweet_data = JSON.stringify(tweets);
            generate_tweet(tweet_data);
        }
    );

    return false;
}

/* Minify tweets returned by twitter: only keep the fields we really need */
function minify_tweets(tweets) {
    for(var i in tweets) {
        /* Created a new tweet object, with only the needed fields. Then
         * set this in the array that we will send to the server. */
        var tweet = {"text": tweets[i].text}
        tweets[i] = tweet;
    }
}

function set_user(u) {
    $('#user').html('Getting user...');
    $('#usernamefield').val(u);
    $.getJSON('http://api.twitter.com/1/users/show.json?screen_name=' + u + '&callback=?',
        function(j) {
            $('#user').html('Processing user...');
            $.post('user/', {data: JSON.stringify(j)}, function(h) {
                $('#user').html(h);
            });
        }
    );
}

function generate_tweet(d) {
    $('#tweet').html('Generating tweet...');
    $.post('tweet/', {data: d, user: user}, function(h) {
        $('#tweet').html(h);
    });
}
