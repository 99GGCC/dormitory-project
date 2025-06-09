package com.dormitory.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import com.dormitory.controller.dto.NoticeInfoDTO;
import com.dormitory.controller.qry.NoticeInfoQry;
import com.dormitory.controller.vo.NoticeInfoVO;
import com.dormitory.entity.NoticeInfo;

import java.util.List;

/**
 * <p>
 * 公告信息表 服务类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface NoticeInfoService extends IService<NoticeInfo> {
    /**
     * 公告信息分页查询
     *
     * @param qry 查询Qry
     * @return IPage<NoticeInfoVO>
     */
    IPage<NoticeInfoVO> pageByQry(NoticeInfoQry qry);

    /**
     * 公告信息列表查询
     *
     * @param qry 查询Qry
     * @return List<NoticeInfoVO>
     */
    List<NoticeInfoVO> listByQry(NoticeInfoQry qry);

    /**
     * 公告信息详情
     *
     * @param noticeId 公告ID
     * @return NoticeInfoVO
     */
    NoticeInfoVO detailById(Long noticeId);

    /**
     * 新增公告信息
     *
     * @param dto 公告DTO
     * @return Boolean
     */
    Boolean add(NoticeInfoDTO dto);

    /**
     * 编辑公告信息
     *
     * @param noticeId 公告ID
     * @param dto       公告DTO
     * @return Boolean
     */
    Boolean edit(Long noticeId, NoticeInfoDTO dto);

    /**
     * 删除公告信息
     *
     * @param noticeId 公告ID
     * @return Boolean
     */
    Boolean del(Long noticeId);
}
