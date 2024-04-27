

-- setup 
USE FIELD_2024_NOLAatDUMMERSEE; 

CREATE TABLE AUTHORS (
  Name char(50)                DEFAULT NULL COMMENT 'Your full name.',
  author varchar(3)            DEFAULT NULL COMMENT 'Your initials. Use two or three letters. This is what you will use when entering data.',
  START date                   DEFAULT NULL COMMENT 'Start of work (YYYY-MM-DD)',
  STOP date                    DEFAULT NULL COMMENT 'End of work (YYYY-MM-DD)',
  gps_id int(2)                DEFAULT NULL COMMENT 'Personal GPS ID',
  radio_id int(2)              DEFAULT NULL COMMENT 'Personal radio ID',
  cam_id int(2)                DEFAULT NULL COMMENT 'Personal camcorder ID',
  remarks varchar(50)          DEFAULT NULL COMMENT 'Please provide any relevant information, such as your ratings in cooking, chess, or poker.'
) ENGINE=ARIA;

CREATE TABLE CAPTURES (
  species varchar(50)          DEFAULT NULL COMMENT '4 letter code, e.g. NOLA',
  date date                    DEFAULT NULL COMMENT 'Date caught YYYY-MM-DD',
  cap_start time               DEFAULT NULL COMMENT 'hh:mm capture start',
  caught    time               DEFAULT NULL COMMENT 'hh:mm time caught ',
  released time                DEFAULT NULL COMMENT 'hh:mm time released',
  capture_meth char(1)         DEFAULT NULL COMMENT 'T (Trap), M (Mistnet), O (Other)',
  nest VARCHAR(7)              DEFAULT NULL COMMENT '<kbd>Nest ID</kbd>, see NESTS table'
  form_id int(3)               DEFAULT NULL COMMENT 'Capture form ID',
  author varchar(3)            DEFAULT NULL COMMENT 'Initials of the bander (see authors table).',
  gps_id int(2)                DEFAULT NULL COMMENT 'GPS ID (1-12)',
  gps_point int(4)             DEFAULT NULL COMMENT 'Waypoint number of the capture location',
  
  sex_observed char(1)         DEFAULT NULL COMMENT 'The observed sex',
  ID varchar(255)              DEFAULT NULL COMMENT 'Metal ring number.',
  UL varchar(10)               DEFAULT NULL COMMENT 'color band',
  UR varchar(10)               DEFAULT NULL COMMENT 'color band',
  LL varchar(10)               DEFAULT NULL COMMENT 'color band',
  LR varchar(10)               DEFAULT NULL COMMENT 'color band',
  tagID varchar(10)            DEFAULT NULL COMMENT  'tagID',
  tag_action varchar(10)       DEFAULT NULL COMMENT 'on (tag deployed) or off(tag removed)',

  recapture BOOLEAN            DEFAULT NULL COMMENT '1(yes), 0(no)',
  
  tarsus DOUBLE                DEFAULT NULL COMMENT 'Length (mm)',
  culmen DOUBLE                DEFAULT NULL COMMENT 'Length (mm)',
  total_head DOUBLE            DEFAULT NULL COMMENT 'Length (mm)',
  crest DOUBLE                 DEFAULT NULL COMMENT 'Length (mm)',
  wing DOUBLE                  DEFAULT NULL COMMENT 'Length (mm)',
  weight DOUBLE                DEFAULT NULL COMMENT 'Body mass (g)',
  wt_w_tag BOOLEAN             DEFAULT NULL COMMENT '0(no) or 1(yes).Was the body weight measured with the tag on?',
  samples    varchar(255)      DEFAULT NULL COMMENT 'Comma delimited: F(fecal), C(cloaca_swab), W(blood whole), S(blood smear), P(blood plasma)',
  
  cam_id    varchar(255)       DEFAULT NULL COMMENT 'Camera ID',
  photo_start varchar(255)     DEFAULT NULL COMMENT 'ID photo start',
  photo_end varchar(255)       DEFAULT NULL COMMENT 'ID photo end',


  
  dead BOOLEAN                 DEFAULT NULL COMMENT '1(yes). Write in the comments the cause of death.',
  comments varchar(255)        DEFAULT NULL COMMENT 'Comments',
  
  nov int(1) unsigned zerofill DEFAULT NULL COMMENT 'Validation flag. Do not edit this column !',
  pk int(10) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (pk),
  KEY ID (ID),
  KEY tagID (tagID),
  KEY COMBO (UL,LL,UR,LR),
  KEY gps (gps_id,gps_point)
) ENGINE=ARIA;

CREATE TABLE RESIGHTINGS (
  species varchar(50)         DEFAULT NULL COMMENT '4 letter code, e.g. NOLA.',
  author varchar(3)            DEFAULT NULL COMMENT 'Initials of the observer (see authors table).',
  gps_id int(2)                DEFAULT NULL COMMENT 'GPS ID',
  gps_point_start int(3)       DEFAULT NULL COMMENT 'GPS waypoint for the start of the behaviour (required)',
  gps_point_stop int(3)        DEFAULT NULL COMMENT 'GPS waypoint for the stop of the behaviour (optional)',
  rclass varchar(2)            DEFAULT NULL COMMENT 'Resighting class. "C" (pre-capture), "V" (video session), "R" (regular)',

  UL varchar(10)               DEFAULT NULL COMMENT 'color band',
  UR varchar(10)               DEFAULT NULL COMMENT 'color band',
  LL varchar(10)               DEFAULT NULL COMMENT 'color band',
  LR varchar(10)               DEFAULT NULL COMMENT 'color band',

  behaviour TEXT               DEFAULT NULL COMMENT 'description of the behaviour or, when a protocol exist, behavioral class.' ,

  cam_id integer(2) DEFAULT NULL COMMENT 'camera ID',

  comments varchar(500)        DEFAULT NULL COMMENT 'Comments',
  
  nov int(1) unsigned zerofill DEFAULT NULL COMMENT 'No validation. Do not edit !',
  pk int(10) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (pk),
  KEY COMBO (UL,LL,UR,LR),
  KEY gps1 (gps_id,gps_point_start),
  KEY gps2 (gps_id,gps_point_stop)
) ENGINE=ARIA;

CREATE TABLE GPS_POINTS (
  gps_id int(2) NOT NULL        COMMENT 'gps id',
  gps_point int(10) NOT NULL    COMMENT 'gps point',
  datetime_ datetime NOT NULL   COMMENT 'gps date-time (CET)',
  lat double NOT NULL           COMMENT 'latitude',
  lon double NOT NULL           COMMENT 'longitude',
  ele double NOT NULL           COMMENT 'elevation',
  pk int(10) NOT NULL           AUTO_INCREMENT,
  PRIMARY KEY (pk),
  KEY gps (gps_id,gps_point),
  KEY datetime_ (datetime_)
) ENGINE=ARIA;

CREATE TABLE GPS_TRACKS (
  gps_id int(2) NOT NULL        COMMENT 'gps id',
  seg_id int(10) NOT NULL       COMMENT 'segment id',
  seg_point_id int(10) NOT NULL COMMENT 'segment point id',
  datetime_ datetime NOT NULL   COMMENT 'gps date-time (CET)',
  lat double NOT NULL           COMMENT 'latitude',
  lon double NOT NULL           COMMENT 'longitude',
  ele float NOT NULL            COMMENT 'elevation',
  pk int(10) NOT NULL           AUTO_INCREMENT,
  PRIMARY KEY (pk),
  KEY gps (gps_id,seg_id),
  KEY datetime_ (datetime_)
) ENGINE=ARIA;

CREATE TABLE COMBOS (
  species varchar(50) DEFAULT NULL COMMENT  '4 letter code, e.g. NOLA',
  UL varchar(10)      DEFAULT NULL COMMENT  'Upper left',
  UR varchar(10)      DEFAULT NULL COMMENT  'Upper right',
  LL varchar(10)      DEFAULT NULL COMMENT  'Lower left',
  LR varchar(10)      DEFAULT NULL COMMENT  'Lower right',
  
  pk int(10) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (pk),
  KEY COMBO (UL,LL,UR,LR)
) ENGINE=ARIA;

CREATE TABLE NESTS (
  author varchar(3)            DEFAULT NULL COMMENT 'Initials (see authors file)',
  species varchar(50)          DEFAULT NULL COMMENT '4 letter code, e.g. NOLA',
  nest varchar(7)              DEFAULT NULL COMMENT 'Nest ID: SXYY, Where S is Species(the first letter of the species name), X=your GPS number, YY=your running nest number',
  nest_state varchar(5)        DEFAULT NULL COMMENT '<b>F</b> (found), <b>C</b> (collected), <b>I</b> (Incubated, active nest), <b>(p)P</b> [(possibly)Predated], <b>(p)D</b> [(possibly)Deserted], <b>H</b> (hatched, received hatched chicks), <b>notA</b> (not Active, nest marks removed. )',
  gps_id int(2)                DEFAULT NULL COMMENT 'GPS ID',
  gps_point int(3)             DEFAULT NULL COMMENT 'GPS waypoint (if nest_state is F this is the position of the nest, otherwise this is the escape distance)',
  date date                    DEFAULT NULL COMMENT 'Date (YYYY-MM-DD) when the nest was checked.',
  time_appr time               DEFAULT NULL COMMENT 'Time (HH:MM) when the nest was checked.',
  clutch_size int(1)           DEFAULT NULL COMMENT 'Clutch size',
  female_ID int(10)            DEFAULT NULL COMMENT 'Metal ID of incubating female (this is entered once the COMBO of the female 1s 100% sure (e.g. confirmed twice independently))',
  male_ID int(10)              DEFAULT NULL COMMENT 'Metal ID of incubating male (this is entered once the COMBO of the male 1s 100% sure (e.g. confirmed twice independently))',
  f_UL varchar(10)             DEFAULT NULL COMMENT 'color band',
  f_UR varchar(10)             DEFAULT NULL COMMENT 'color band',
  f_LL varchar(10)             DEFAULT NULL COMMENT 'color band',
  f_LR varchar(10)             DEFAULT NULL COMMENT 'color band',
  m_UL varchar(10)             DEFAULT NULL COMMENT 'color band',
  m_UR varchar(10)             DEFAULT NULL COMMENT 'color band',
  m_LL varchar(10)             DEFAULT NULL COMMENT 'color band',
  m_LR varchar(10)             DEFAULT NULL COMMENT 'color band',
  behav varchar(50)            DEFAULT NULL COMMENT 'Behaviour of the incubating bird (comma seperated): INC=Incubating, DF=Driving Flight, BW=Distraction Display (e.g. Broken Wing) or O=Other behaviours (describe in comments)',
  comments varchar(500)        DEFAULT NULL COMMENT 'Comments',
  nov int(1) unsigned zerofill DEFAULT NULL COMMENT 'No validation. Do not edit !',
  pk int(10) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (pk),
  KEY nest (nest),
  KEY species (species),
  KEY gps (gps_id,gps_point)
) ENGINE=Aria ;

CREATE TABLE EGGS (
  species varchar(50)          DEFAULT NULL COMMENT '4 letter code, e.g. NOLA.',
  date date                    DEFAULT NULL COMMENT 'Date (YYYY-MM-DD) when the nest was checked.',
  nest varchar(7)              DEFAULT NULL COMMENT '<kbd>Nest ID</kbd>, see NESTS table',
  float_angle int(3)           DEFAULT NULL COMMENT 'Egg floating angle (degrees).',    
  surface int(3)               DEFAULT NULL COMMENT 'Egg surface (mm).',    
  comments varchar(500)        DEFAULT NULL COMMENT 'Comments.',
  nov int(1) unsigned zerofill DEFAULT NULL COMMENT 'No validation. Do not edit !',
  pk int(10) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (pk),
  KEY nest (nest),
  KEY date (date)
) ENGINE=Aria ;

CREATE TABLE CHICKS (
  
  date      date               DEFAULT NULL COMMENT 'Date (YYYY-MM-DD).',
  caught    time               DEFAULT NULL COMMENT 'hh:mm time caught ',
  released  time               DEFAULT NULL COMMENT 'hh:mm time released',
  nest      varchar(7)         DEFAULT NULL COMMENT '<kbd>Nest ID</kbd>, see NESTS table',
  author  varchar(3)           DEFAULT NULL COMMENT 'Initials (see authors file)',
  ID      varchar(255)         DEFAULT NULL COMMENT 'Metal ring number.',   
  tarsus  DOUBLE               DEFAULT NULL COMMENT 'Length (mm)',  
  weight  DOUBLE               DEFAULT NULL COMMENT 'Body mass (g)',
  sample  varchar(255)         DEFAULT NULL COMMENT 'Comma delimited: F(fecal), C(cloaca_swab), W(blood whole), S(blood smear), P(blood plasma)',
  comments  varchar(500)       DEFAULT NULL COMMENT 'Comments.',

  nov int(1) unsigned zerofill DEFAULT NULL COMMENT 'No validation. Do not edit !',
  pk int(10) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (pk),
  KEY nest (nest),
  KEY date (date)
) ENGINE=Aria ;

-- VIEWS
DROP   VIEW IF EXISTS CAPTURES_summary;
CREATE VIEW CAPTURES_summary AS 
  SELECT 
  CAST( CONCAT_WS(' ', date, released) AS DATETIME) datetime_, 
  tagID, 
  UL, LL, UR, LR, 
  recapture, 
  pk
  FROM CAPTURES
   WHERE ID not in (SELECT ID FROM CAPTURES where dead = 1)
  ;

DROP   VIEW IF EXISTS nest_locations;

CREATE VIEW nest_locations AS 
SELECT DISTINCT nest, lat, lon from NESTS n join `GPS_POINTS` g on n.gps_id = g.gps_id AND n.gps_point = g.gps_point;
