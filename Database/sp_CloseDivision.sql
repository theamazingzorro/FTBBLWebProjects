USE ftbbl;
DROP PROCEDURE IF EXISTS sp_CloseDivision;
DELIMITER //
CREATE PROCEDURE sp_CloseDivision (IN i_div_id INT)
BEGIN

	UPDATE TeamDivision
    SET end_date = CURDATE()
    WHERE div_id = i_div_id
    AND end_date IS NULL;
    
    UPDATE Division
    SET closed = True
    WHERE id = i_div_id;

END//
DELIMITER ;