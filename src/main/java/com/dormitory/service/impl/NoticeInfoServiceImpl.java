package com.dormitory.service.impl;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.conditions.query.LambdaQueryChainWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.dormitory.controller.dto.NoticeInfoDTO;
import com.dormitory.controller.qry.NoticeInfoQry;
import com.dormitory.controller.vo.NoticeInfoVO;
import com.dormitory.entity.NoticeInfo;
import com.dormitory.exception.ServiceException;
import com.dormitory.mapper.NoticeInfoMapper;
import com.dormitory.service.NoticeInfoService;
import com.dormitory.utils.CopyUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * 公告信息表 服务实现类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Service
public class NoticeInfoServiceImpl extends ServiceImpl<NoticeInfoMapper, NoticeInfo> implements NoticeInfoService {
    /**
     * 公告信息分页查询
     *
     * @param qry 查询Qry
     * @return IPage<NoticeInfoVO>
     */
    @Override
    public IPage<NoticeInfoVO> pageByQry(NoticeInfoQry qry) {
        IPage<NoticeInfo> pages = new Page<>(qry.getPage(), qry.getLimit());
        pages = new LambdaQueryChainWrapper<>(baseMapper)
                .like(StringUtils.isNotEmpty(qry.getNoticeTitle()), NoticeInfo::getNoticeTitle, qry.getNoticeTitle())
                .like(StringUtils.isNotEmpty(qry.getNoticeContent()), NoticeInfo::getNoticeContent, qry.getNoticeContent())
                .page(pages);
        return CopyUtils.covertPage(pages, NoticeInfoVO.class);
    }

    /**
     * 公告信息列表查询
     *
     * @param qry 查询Qry
     * @return List<NoticeInfoVO>
     */
    @Override
    public List<NoticeInfoVO> listByQry(NoticeInfoQry qry) {
        List<NoticeInfo> list = new LambdaQueryChainWrapper<>(baseMapper)
                .like(StringUtils.isNotEmpty(qry.getNoticeTitle()), NoticeInfo::getNoticeTitle, qry.getNoticeTitle())
                .list();
        return CopyUtils.classCopyList(list, NoticeInfoVO.class);
    }

    /**
     * 公告信息详情
     *
     * @param noticeId 公告ID
     * @return NoticeInfoVO
     */
    @Override
    public NoticeInfoVO detailById(Long noticeId) {
        return CopyUtils.classCopy(baseMapper.selectById(noticeId), NoticeInfoVO.class);
    }

    /**
     * 新增公告信息
     *
     * @param dto 公告DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean add(NoticeInfoDTO dto) {
        // 判断公告信息是否存在
        List<NoticeInfo> list = new LambdaQueryChainWrapper<>(baseMapper)
                .eq(NoticeInfo::getNoticeTitle, dto.getNoticeTitle())
                .list();
        if (!CollectionUtils.isEmpty(list)) {
            throw new ServiceException("公告信息已存在,新增失败!");
        }
        // 复制公告信息实体
        NoticeInfo noticeInfo = CopyUtils.classCopy(dto, NoticeInfo.class);
        // 设置公告时间
        noticeInfo.setNoticeTime(new Date());
        // 保存公告信息，并返回结果
        return baseMapper.insert(noticeInfo) > 0;
    }

    /**
     * 编辑公告信息
     *
     * @param noticeId 公告ID
     * @param dto       公告DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean edit(Long noticeId, NoticeInfoDTO dto) {
        // 判断公告信息是否存在
        List<NoticeInfo> list = new LambdaQueryChainWrapper<>(baseMapper)
                .eq(NoticeInfo::getNoticeTitle, dto.getNoticeTitle())
                .ne(NoticeInfo::getNoticeId, noticeId)
                .list();
        if (!CollectionUtils.isEmpty(list)) {
            throw new ServiceException("公告信息已存在,编辑失败!");
        }
        // 复制公告信息实体
        NoticeInfo noticeInfo = CopyUtils.classCopy(dto, NoticeInfo.class);
        // 设置公告ID
        noticeInfo.setNoticeId(noticeId);
        // 编辑公告信息，并返回结果
        return baseMapper.updateById(noticeInfo) > 0;
    }

    /**
     * 删除公告信息
     *
     * @param noticeId 公告ID
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean del(Long noticeId) {
        // 删除公告信息，并返回结果
        return baseMapper.deleteById(noticeId) > 0;
    }
}
