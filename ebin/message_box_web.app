{application, message_box_web,
 [{description, "message_box_web"},
  {vsn, "0.01"},
  {modules, [
    message_box_web,
    message_box_web_app,
    message_box_web_sup,
    message_box_web_web,
    message_box_web_deps
  ]},
  {registered, []},
  {mod, {message_box_web_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
