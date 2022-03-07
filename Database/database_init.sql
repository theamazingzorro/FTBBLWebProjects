create database if not exists ftbbl;
use ftbbl;

create table if not exists Team(
   id INT NOT NULL AUTO_INCREMENT,
   name VARCHAR(40) NOT NULL,
   race VARCHAR(40) NOT NULL,
   coach VARCHAR(40) NOT NULL,
   is_active BOOL NOT NULL,
   PRIMARY KEY ( id )
);