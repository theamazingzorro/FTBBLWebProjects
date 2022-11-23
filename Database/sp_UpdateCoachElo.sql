USE ftbbl;

DROP PROCEDURE IF EXISTS sp_UpdateCoachElo;
DELIMITER //
CREATE PROCEDURE sp_UpdateCoachElo (IN i_coach_id INT, IN i_elo INT)
BEGIN
	INSERT INTO CoachEloHistory (coach_id, elo, date) VALUES (i_coach_id, i_elo, NOW());
    
    UPDATE Coach
    SET elo = i_elo
    WHERE id = i_coach_id;
END//
DELIMITER ;