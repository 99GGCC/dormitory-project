package com.dormitory.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import com.dormitory.controller.dto.SysDictDTO;
import com.dormitory.controller.qry.SysDictQry;
import com.dormitory.controller.vo.SysDictVO;
import com.dormitory.entity.SysDict;

import java.util.List;

/**
 * <p>
 * 字典表 服务类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface SysDictService extends IService<SysDict> {

    /**
     * 字典信息分页查询
     *
     * @param qry 查询qry
     * @return IPage<SysDictVO>
     */
    IPage<SysDictVO> pageByQry(SysDictQry qry);

    /**
     * 字典信息列表查询
     *
     * @param qry 查询qry
     * @return List<SysDictVO>
     */
    List<SysDictVO> listByQry(SysDictQry qry);

    /**
     * 字典信息详情
     *
     * @param dictId 字典ID
     * @return SysDictVO
     */
    SysDictVO detailById(Long dictId);

    /**
     * 新增字典信息
     *
     * @param dto 字典dto
     * @return Boolean
     */
    Boolean add(SysDictDTO dto);

    /**
     * 编辑字典信息
     *
     * @param dictId 字典ID
     * @param dto    字典dto
     * @return Boolean
     */
    Boolean edit(Long dictId, SysDictDTO dto);

    /**
     * 删除字典信息
     *
     * @param dictId 字典ID
     * @return Boolean
     */
    Boolean del(Long dictId);

    /**
     * 字典信息缓存
     *
     * @return Object
     */
    Object cache();
}
