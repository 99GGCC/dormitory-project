package com.dormitory.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.dormitory.entity.SignInIssue;
import org.apache.ibatis.annotations.Param;

/**
 * <p>
 * 考勤发布表 Mapper 接口
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface SignInIssueMapper extends BaseMapper<SignInIssue> {

    /**
     * 更新签到学生人数
     *
     * @param signInId 签到ID
     * @return Boolean
     */
    Boolean addNum(@Param("signInId") Long signInId);
}
