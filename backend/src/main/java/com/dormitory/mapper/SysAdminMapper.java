package com.dormitory.mapper;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.dormitory.controller.qry.AdminQry;
import com.dormitory.controller.vo.AdminVO;
import com.dormitory.entity.SysAdmin;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

/**
 * <p>
 * 管理员表 Mapper 接口
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface SysAdminMapper extends BaseMapper<SysAdmin> {

    /**
     * 管理员分页查询
     *
     * @param adminQry 查询Qry
     * @return IPage<AdminVO>
     */
    IPage<AdminVO> pageByQry(@Param("adminQry") AdminQry adminQry, @Param("page") Page<AdminVO> page);

    /**
     * 管理员信息详情
     *
     * @param adminId 管理员ID
     * @return AdminVO
     */
    AdminVO detail(@Param("adminId") Long adminId);
}
