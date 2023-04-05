/*

-----------------------------------------------------------------------------------------------------------------------------------
													    Guidelines
-----------------------------------------------------------------------------------------------------------------------------------

The provided document is a guide for the project. Follow the instructions and take the necessary steps to finish
the project in the SQL file			

-----------------------------------------------------------------------------------------------------------------------------------
                                                         Queries
                                               
-----------------------------------------------------------------------------------------------------------------------------------*/
  
/*-- QUESTIONS RELATED TO CUSTOMERS
     [Q1] What is the distribution of customers across states?
     Hint: For each state, count the number of customers.*/
USE new_wheels;

SELECT state, COUNT(*) customers_by_state
FROM customer_t
GROUP BY state
ORDER BY customers_by_state DESC;

-- ---------------------------------------------------------------------------------------------------------------------------------

/* [Q2] What is the average rating in each quarter?
-- Very Bad is 1, Bad is 2, Okay is 3, Good is 4, Very Good is 5.

Hint: Use a common table expression and in that CTE, assign numbers to the different customer ratings. 
      Now average the feedback for each quarter. 

Note: For reference, refer to question number 4. Week-2: mls_week-2_gl-beats_solution-1.sql. 
      You'll get an overview of how to use common table expressions from this question.*/

WITH ratings AS
(SELECT CASE WHEN customer_feedback = 'Very Bad' THEN 1
WHEN customer_feedback = 'Bad' THEN 2
WHEN customer_feedback = 'Okay' THEN 3
WHEN customer_feedback = 'Good' THEN 4
ELSE 5
END
AS cust_ratings, quarter_number
FROM order_t)

SELECT quarter_number,
	AVG(cust_ratings) AS avg_rating FROM ratings
GROUP BY quarter_number
ORDER BY quarter_number ASC;

-- ---------------------------------------------------------------------------------------------------------------------------------

/* [Q3] Are customers getting more dissatisfied over time?

Hint: Need the percentage of different types of customer feedback in each quarter. Use a common table expression and
	  determine the number of customer feedback in each category as well as the total number of customer feedback in each quarter.
	  Now use that common table expression to find out the percentage of different types of customer feedback in each quarter.
      Eg: (total number of very good feedback/total customer feedback)* 100 gives you the percentage of very good feedback.
      
Note: For reference, refer to question number 4. Week-2: mls_week-2_gl-beats_solution-1.sql. 
      You'll get an overview of how to use common table expressions from this question.*/
      
WITH fb AS
(SELECT quarter_number,
customer_feedback,
COUNT(customer_feedback) AS feedback_count,
SUM(COUNT(customer_feedback)) OVER (PARTITION BY quarter_number) AS total_feedback
FROM order_t
GROUP BY quarter_number, customer_feedback)

SELECT *,
	feedback_count / total_feedback * 100 AS feedback_percent
FROM fb
GROUP BY quarter_number, customer_feedback
ORDER BY quarter_number ASC,
	FIELD(customer_feedback, 'Very Bad', 'Bad', 'Okay', 'Good', 'Very Good');

-- ---------------------------------------------------------------------------------------------------------------------------------

/*[Q4] Which are the top 5 vehicle makers preferred by the customer.

Hint: For each vehicle make what is the count of the customers.*/

WITH cust_vehicle AS
(SELECT
	p.vehicle_maker,
    o.customer_id
FROM product_t AS p
	INNER JOIN
    order_t AS o
    ON p.product_id = o.product_id
    INNER JOIN
    customer_t as c
    ON o.customer_id = c.customer_id)

SELECT vehicle_maker,
	COUNT(customer_id) AS num_customers
FROM cust_vehicle
GROUP BY vehicle_maker
ORDER BY num_customers DESC
LIMIT 5;

-- ---------------------------------------------------------------------------------------------------------------------------------

/*[Q5] What is the most preferred vehicle make in each state?

Hint: Use the window function RANK() to rank based on the count of customers for each state and vehicle maker. 
After ranking, take the vehicle maker whose rank is 1.*/

WITH cust_vehicle AS
(SELECT
	p.vehicle_maker,
    c.state,
    o.customer_id
FROM product_t AS p
	INNER JOIN
    order_t AS o
    ON p.product_id = o.product_id
    INNER JOIN
    customer_t AS c
    ON o.customer_id = c.customer_id)

SELECT *
FROM
	(SELECT
		vehicle_maker,
		state,
		COUNT(customer_id) AS cust_count,
		RANK() OVER (PARTITION BY state ORDER BY COUNT(vehicle_maker) DESC) AS maker_rank
	FROM cust_vehicle
	GROUP BY vehicle_maker, state) AS ranks
WHERE maker_rank = 1;

-- ---------------------------------------------------------------------------------------------------------------------------------

/*QUESTIONS RELATED TO REVENUE and ORDERS 

-- [Q6] What is the trend of number of orders by quarters?

Hint: Count the number of orders for each quarter.*/

SELECT
	quarter_number,
    COUNT(order_id) AS orders
FROM order_t
GROUP BY quarter_number
ORDER BY quarter_number ASC;

-- ---------------------------------------------------------------------------------------------------------------------------------

/* [Q7] What is the quarter over quarter % change in revenue? 

Hint: Quarter over Quarter percentage change in revenue means what is the change in revenue from the subsequent quarter to the previous quarter in percentage.
      To calculate you need to use the common table expression to find out the sum of revenue for each quarter.
      Then use that CTE along with the LAG function to calculate the QoQ percentage change in revenue.
*/

WITH rev AS
(SELECT
	quarter_number,
    SUM(ROUND(quantity*vehicle_price - quantity*vehicle_price*discount, 2)) AS revenue,
    LAG(SUM(ROUND(quantity*vehicle_price - quantity*vehicle_price*discount, 2))) OVER (ORDER BY quarter_number) AS previous_revenue
FROM order_t
GROUP BY quarter_number
ORDER BY quarter_number ASC)

SELECT
	*,
    ((revenue - previous_revenue)/previous_revenue * 100) AS pct_chng_rev
FROM rev;
-- ---------------------------------------------------------------------------------------------------------------------------------

/* [Q8] What is the trend of revenue and orders by quarters?

Hint: Find out the sum of revenue and count the number of orders for each quarter.*/

WITH rev AS
(SELECT
	customer_id,
    order_id,
    quarter_number,
    ROUND(SUM(quantity*vehicle_price - quantity*vehicle_price*discount), 2) AS revenue
FROM order_t
GROUP BY customer_id, order_id)

SELECT
	quarter_number,
    SUM(revenue) AS total_revenue,
    COUNT(order_id) AS order_count
FROM rev
GROUP BY quarter_number
ORDER BY quarter_number ASC;

-- ---------------------------------------------------------------------------------------------------------------------------------

/* QUESTIONS RELATED TO SHIPPING 
    [Q9] What is the average discount offered for different types of credit cards?

Hint: Find out the average of discount for each credit card type.*/

WITH cust_orders AS
(SELECT
	discount,
    credit_card_type
FROM order_t AS o
	INNER JOIN
    customer_t AS c
    ON o.customer_id = c.customer_id)

SELECT
	credit_card_type,
    AVG(discount) AS avg_discount
FROM cust_orders
GROUP BY credit_card_type;

-- ---------------------------------------------------------------------------------------------------------------------------------

/* [Q10] What is the average time taken to ship the placed orders for each quarters?
	Hint: Use the datediff function to find the difference between the ship date and the order date.
*/

SELECT
	quarter_number,
    AVG(DATEDIFF(ship_date, order_date)) AS avg_days_to_ship
FROM order_t
GROUP BY quarter_number
ORDER BY quarter_number ASC;


/* Business Overview */
/* Total Revenue */
WITH rev AS
(SELECT
	quarter_number,
    SUM(ROUND(quantity*vehicle_price - quantity*vehicle_price*discount, 2)) AS revenue,
    LAG(SUM(ROUND(quantity*vehicle_price - quantity*vehicle_price*discount, 2))) OVER (ORDER BY quarter_number) AS previous_revenue
FROM order_t
GROUP BY quarter_number
ORDER BY quarter_number ASC)

SELECT
	SUM(revenue) AS total_rev
FROM rev;

/* Total Orders */
SELECT
	COUNT(order_id)
FROM order_t;

/* Total Customers */
SELECT
	COUNT(DISTINCT customer_id)
FROM order_t;

/* Average Rating */
WITH ratings AS
(SELECT CASE WHEN customer_feedback = 'Very Bad' THEN 1
WHEN customer_feedback = 'Bad' THEN 2
WHEN customer_feedback = 'Okay' THEN 3
WHEN customer_feedback = 'Good' THEN 4
ELSE 5
END
AS cust_ratings, quarter_number
FROM order_t)

SELECT AVG(cust_ratings) FROM ratings;

/* Average Days to Ship */
SELECT
    AVG(DATEDIFF(ship_date, order_date)) AS avg_days_to_ship
FROM order_t;
-- --------------------------------------------------------Done----------------------------------------------------------------------
-- ----------------------------------------------------------------------------------------------------------------------------------