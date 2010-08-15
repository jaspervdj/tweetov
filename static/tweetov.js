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
        function(j) {
            tweet_data = $.toJSON(j);
            generate_tweet(tweet_data);
        }
    );

    return false;
}

function set_user(u) {
    $('#user').html('Getting user...');
    $('#usernamefield').val(u);
    $.getJSON('http://api.twitter.com/1/users/show.json?screen_name=' + u + '&callback=?',
        function(j) {
            $('#user').html('Processing user...');
            $.get('user/', {data: $.toJSON(j)}, function(h) {
                $('#user').html(h);
            });
        }
    );
}

function generate_tweet(d) {
    $('#tweet').html('Generating tweet...');
    $.get('tweet/', {data: d, user: user}, function(h) {
        $('#tweet').html(h);
    });
}
