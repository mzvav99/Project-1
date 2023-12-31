/* INFO521: SQL Project */
/* Project Assignment: Analyzing NYC Restaurant Inspection Results dataset to answer 5 research questions */
/* Data Cleaning conducted in R Studio */

/* Creating Restaurant Table */
CREATE TABLE `Restaurants` (
  `Rest_ID` int NOT NULL,
  `Cuisine` varchar(100) NOT NULL,
  `Borough` varchar(45) NOT NULL,
  PRIMARY KEY (`Rest_ID`),
  UNIQUE KEY `Rest_ID_UNIQUE` (`Rest_ID`),
  KEY `idx_rest_id` (`Rest_ID`),
  KEY `idx_rest_id1` (`Rest_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci; 

/* Creating Inspection Table  */
CREATE TABLE `Inspections` (
  `Inspection_ID` int NOT NULL,
  `Rest_ID` int NOT NULL,
  `Inspection_Type` varchar(500) NOT NULL,
  `Grade` char(1) NOT NULL,
  `Score` int NOT NULL,
  `Critical_Flag` varchar(45) NOT NULL,
  PRIMARY KEY (`Inspection_ID`),
  UNIQUE KEY `Inspection_ID_UNIQUE` (`Inspection_ID`),
  KEY `fk_rest1` (`Rest_ID`),
  CONSTRAINT `fk_rest1` FOREIGN KEY (`Rest_ID`) REFERENCES `Restaurants` (`Rest_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

/* Creating Violations Table */
CREATE TABLE `Violations` (
  `Violation_ID` int NOT NULL,
  `Rest_ID` int NOT NULL,
  `Violation_Type` longtext NOT NULL,
  `Violation_Date` date NOT NULL,
  PRIMARY KEY (`Violation_ID`),
  UNIQUE KEY `Violation_ID_UNIQUE` (`Violation_ID`),
  KEY `fk_rest2` (`Rest_ID`),
  CONSTRAINT `fk_rest2` FOREIGN KEY (`Rest_ID`) REFERENCES `Restaurants` (`Rest_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

/* Creating Council Table */
CREATE TABLE `Council` (
  `Council_ID` int NOT NULL,
  `Rest_ID` int NOT NULL,
  `Council_District` int NOT NULL,
  `Community_Board` int NOT NULL,
  PRIMARY KEY (`Council_ID`),
  UNIQUE KEY `Council_ID_UNIQUE` (`Council_ID`),
  KEY `fk_rest` (`Rest_ID`),
  CONSTRAINT `fk_rest` FOREIGN KEY (`Rest_ID`) REFERENCES `Restaurants` (`Rest_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

/* Adding foreign key to each table */
CREATE INDEX idx_rest_id ON Restaurants(rest_id);
ALTER TABLE Council
ADD CONSTRAINT fk_rest
  FOREIGN KEY (rest_id) REFERENCES Restaurants(rest_id);

ALTER TABLE Inspections
ADD CONSTRAINT fk_rest1
  FOREIGN KEY (rest_id) REFERENCES Restaurants(rest_id);

ALTER TABLE Violations
ADD CONSTRAINT fk_rest2
  FOREIGN KEY (rest_id) REFERENCES Restaurants(rest_id);


/* Question 1. What was the breakdown of restaurant, borough, and inspection grade? */
SELECT r.rest_id, r.borough, i.grade
FROM restaurants r
JOIN inspections i ON r.rest_id = i.rest_id
GROUP BY r.rest_id, r.borough, i.grade
ORDER BY r.rest_id, r.borough, i.grade;

/*Question 2. Top 3 Violation_Types per Borough */
/* Staten Island */
SELECT violation_type, COUNT(*) AS num_violations
FROM restaurants
JOIN violations ON restaurants.rest_id = violations.rest_id
WHERE borough = 'Staten Island'
GROUP BY violation_type
ORDER BY num_violations DESC
LIMIT 3;

/*Bronx*/
SELECT violation_type, COUNT(*) AS num_violations
FROM restaurants
JOIN violations ON restaurants.rest_id = violations.rest_id
WHERE borough = 'Bronx'
GROUP BY violation_type
ORDER BY num_violations DESC
LIMIT 3;

/*Manhattan*/
SELECT violation_type, COUNT(*) AS num_violations
FROM restaurants
JOIN violations ON restaurants.rest_id = violations.rest_id
WHERE borough = 'Manhattan'
GROUP BY violation_type
ORDER BY num_violations DESC
LIMIT 3;

/*Brooklyn*/
SELECT violation_type, COUNT(*) AS num_violations
FROM restaurants
JOIN violations ON restaurants.rest_id = violations.rest_id
WHERE borough = 'Brooklyn'
GROUP BY violation_type
ORDER BY num_violations DESC
LIMIT 3;

/*Queens*/
SELECT violation_type, COUNT(*) AS num_violations
FROM restaurants
JOIN violations ON restaurants.rest_id = violations.rest_id
WHERE borough = 'Queens'
GROUP BY violation_type
ORDER BY num_violations DESC
LIMIT 3;

/* Question 3. List of Boroughs by highest percentage of grade A Restaurants */
SELECT r.borough, 
COUNT(DISTINCT r.rest_id) AS num_restaurants, 
COUNT(DISTINCT CASE WHEN i.grade = 'A' THEN i.rest_id END) AS num_a_grade_restaurants, 
COUNT(DISTINCT CASE WHEN i.grade = 'A' THEN i.rest_id END) * 100.0 / COUNT(DISTINCT r.rest_id) AS pct_grade_A
FROM restaurants r
LEFT JOIN inspections i ON r.rest_id = i.rest_id
GROUP BY r.borough
ORDER BY pct_grade_A DESC;

/* Question 4. Which council district had the highest average inspection score among its restaurants? */
SELECT c.council_district, AVG(i.avg_score) AS avg_inspection_score
FROM council c
JOIN (
  SELECT rest_id, AVG(score) AS avg_score
  FROM inspections
  GROUP BY rest_id
) i ON c.rest_id = i.rest_id
GROUP BY c.council_district
ORDER BY avg_inspection_score DESC
LIMIT 1;

/*Question 5. What percentage of re-inspections received a grade of C in each borough? */
SELECT 
  restaurants.borough,
  100.0 * COUNT(CASE WHEN inspections.grade = 'C' THEN 1 END) / 
  COUNT(CASE WHEN inspections.inspection_type = 'Cycle Inspection / Re-inspection' THEN 1 END) AS percentage_C_re_inspection
FROM 
  inspections 
  JOIN restaurants ON inspections.rest_id = restaurants.rest_id
GROUP BY 
  restaurants.borough;

