DROP PROCEDURE IF EXISTS sp_UpdateTeamDivision;
DELIMITER //
CREATE PROCEDURE sp_UpdateTeamDivision (IN i_team_id INT, IN i_div_id INT)
BEGIN

	UPDATE TeamDivision
    SET end_date = CURDATE()
    WHERE team_id = i_team_id
    AND end_date IS NULL;
    
    INSERT INTO TeamDivision (team_id, div_id, start_date, end_date)
    VALUES (i_team_id, i_div_id, CURDATE(), null);

END//
DELIMITER ;