package com.dormitory.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.dormitory.controller.qry.SignInRecordQry;
import com.dormitory.controller.vo.SignInRecordVO;
import com.dormitory.entity.SignInRecord;
import org.apache.ibatis.annotations.Param;

/**
 * <p>
 * 考勤记录表 Mapper 接口
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface SignInRecordMapper extends BaseMapper<SignInRecord> {

    /**
     * 考勤记录分页查询
     *
     * @param page 分页类
     * @param qry  查询Qry
     * @return IPage<SignInRecordVO>
     */
    IPage<SignInRecordVO> pageByQry(@Param("page") Page<SignInRecordVO> page, @Param("qry") SignInRecordQry qry);

    /**
     * 考勤记录详情
     *
     * @param recordId 记录ID
     * @return SignInRecordVO
     */
    SignInRecordVO detailById(@Param("recordId") Long recordId);
}
