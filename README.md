py-configvalidator adds a validation capability to Common Lisp applications using py-configparser.

While py-configparser allows for reading and interpolating .INI-style files, like Python's ConfigParser, it produces raw output or raises low-level conditions.

With py-configvalidator, you can use a similar .INI-style declarative template format to specify basic types and constraints that should apply to a well-formed config file consumed by your application.

The purpose of this is to separate the minutae of basic config file validation from application code, and to expose a richer set of Common Lisp conditions that describe validation failures.

