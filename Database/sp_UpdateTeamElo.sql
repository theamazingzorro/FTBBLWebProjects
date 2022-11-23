USE ftbbl;

DROP PROCEDURE IF EXISTS sp_UpdateTeamElo;
DELIMITER //
CREATE PROCEDURE sp_UpdateTeamElo (IN i_team_id INT, IN i_elo INT)
BEGIN
	INSERT INTO TeamEloHistory (team_id, elo, date) VALUES (i_team_id, i_elo, NOW());
    
    UPDATE Team
    SET elo = i_elo
    WHERE id = i_team_id;
END//
DELIMITER ;