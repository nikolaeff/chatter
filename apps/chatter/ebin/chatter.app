{application,chatter,
             [{description,"Web Chatter"},
              {vsn,"0.0.1"},
              {registered,[]},
              {modules,[chatter_app,chatter_sup,room_server,web_server]},
              {applications,[kernel,stdlib]},
              {included_applications,[misultin]},
              {mod,{chatter_app,[]}},
              {env,[]}]}.