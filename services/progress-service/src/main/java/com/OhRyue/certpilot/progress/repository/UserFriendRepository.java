package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.UserFriend;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface UserFriendRepository extends JpaRepository<UserFriend, Long> {

  List<UserFriend> findByUserIdAndStatus(String userId, String status);
}

