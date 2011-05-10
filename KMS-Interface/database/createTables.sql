CREATE SCHEMA kms_interface;

DROP TABLE kms_interface.transport;

CREATE TABLE kms_interface.transport(
  id varchar(256) not null primary key,
  wsdl varchar(512),
  namespace varchar(256),
  service_name varchar(256),
  port_name varchar(256) 
  );

INSERT INTO kms_interface.transport VALUES('HelloTransport','http://localhost:8080/KMS-Transport/TransportService?wsdl','http://localhost:8080/','TransportService','TransportPort');