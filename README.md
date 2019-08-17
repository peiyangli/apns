apns
=====

An OTP application

https://github.com/inaka/apns4erl.git

Build
-----

    $ rebar3 compile


pem
---
    openssl pkcs12 -clcerts -nokeys -out apns_dev_cert.pem -in apns_dev_cert.p12
    openssl pkcs12 -nocerts -out apns_dev_key.pem -in apns_dev_key.p12


example
-------
    Notification = #{<<"aps">> => #{ <<"alert">> => <<"hello">> , <<"sound">> => <<"default">> , <<"badge">> => 1} }.
    apns_pool:push_notification({<<"1213123121">>, Notification, Headers  = apns:default_headers()}, fun(Rt)->io:format("~p~n", [Rt]), ok end).