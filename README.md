Developers Exercise
-------------------


Requirements:
------------
* MySQL 5.5 server or better
* Emysql 0.4.1 Erlang MySQL driver (https://github.com/Eonblast/Emysql.git)
* Erlang R15B03 Browse https://www.erlang-solutions.com/downloads/download-erlang-otp,  then pick your OS for the correct installer.

Compile the Emysql driver and copy it into your Erlang lib directory, (i.e. /usr/lib/erlang/lib/) as emysql-0.4.1.

MySQL preparation
-----------------
Create a database within MySQL named "batmachine"

```mysql
CREATE DATABASE batmachine;
```

And grant all rights to the user *"batter"*

```mysql
GRANT ALL PRIVILEGES ON batmachine.* to batter@localhost IDENTIFIED BY "batmachine";
```

Then you can start the application by running

```bash
./startme.sh
```

