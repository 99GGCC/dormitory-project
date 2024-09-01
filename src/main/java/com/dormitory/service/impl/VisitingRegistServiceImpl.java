package com.dormitory.service.impl;

import cn.dev33.satoken.stp.StpUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.conditions.query.LambdaQueryChainWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.dormitory.controller.dto.VisitingRegistDTO;
import com.dormitory.controller.qry.VisitingRegistQry;
import com.dormitory.controller.vo.VisitingRegistVO;
import com.dormitory.entity.VisitingRegist;
import com.dormitory.exception.ServiceException;
import com.dormitory.mapper.VisitingRegistMapper;
import com.dormitory.service.VisitingRegistService;
import com.dormitory.utils.CopyUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;

/**
 * <p>
 * 来访登记表 服务实现类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Service
public class VisitingRegistServiceImpl extends ServiceImpl<VisitingRegistMapper, VisitingRegist> implements VisitingRegistService {

    /**
     * 来访登记分页查询
     *
     * @param qry 查询Qry
     * @return IPage<VisitingRegistVO>
     */
    @Override
    public IPage<VisitingRegistVO> pageByQry(VisitingRegistQry qry) {
        IPage<VisitingRegist> pages = new Page<>(qry.getPage(), qry.getLimit());
        pages = new LambdaQueryChainWrapper<>(baseMapper)
                .like(StringUtils.isNotEmpty(qry.getVisitingName()), VisitingRegist::getVisitingName, qry.getVisitingName())
                .like(StringUtils.isNotEmpty(qry.getVisitingReason()), VisitingRegist::getVisitingReason, qry.getVisitingReason())
                .eq(ObjectUtils.isNotEmpty(qry.getAdminId()), VisitingRegist::getAdminId, qry.getAdminId())
                .orderByDesc(VisitingRegist::getRegistTime)
                .page(pages);
        return CopyUtils.covertPage(pages, VisitingRegistVO.class);
    }

    /**
     * 来访登记详情
     *
     * @param registId 登记ID
     * @return VisitingRegistVO
     */
    @Override
    public VisitingRegistVO detailById(Long registId) {
        VisitingRegist visitingRegist = baseMapper.selectById(registId);
        return CopyUtils.classCopy(visitingRegist, VisitingRegistVO.class);
    }

    /**
     * 新增来访登记信息
     *
     * @param dto 登记DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean add(VisitingRegistDTO dto) {
        // 组装来访登记实体
        VisitingRegist visitingRegist = CopyUtils.classCopy(dto, VisitingRegist.class);
        // 补全登记时间、值班人员ID
        visitingRegist.setRegistTime(new Date())
                .setAdminId(StpUtil.getLoginIdAsLong());
        // 保存来访登记
        return baseMapper.insert(visitingRegist) > 0;
    }

    /**
     * 编辑来访登记信息
     *
     * @param registId 登记ID
     * @param dto      登记DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean edit(Long registId, VisitingRegistDTO dto) {
        // 根据登记ID和值班人员查询登记信息
        VisitingRegist visitingRegist = new LambdaQueryChainWrapper<>(baseMapper)
                .eq(VisitingRegist::getRegistId, registId)
                .eq(VisitingRegist::getAdminId, StpUtil.getLoginIdAsLong())
                .one();
        if (ObjectUtils.isEmpty(visitingRegist)) {
            throw new ServiceException("不能修改其他管理员的登记信息·");
        }
        // 组装来访登记实体
        visitingRegist = CopyUtils.classCopy(dto, VisitingRegist.class);
        // 补全登记ID
        visitingRegist.setRegistId(registId);
        // 编辑来访登记
        return baseMapper.updateById(visitingRegist) > 0;
    }

    /**
     * 删除来访登记
     *
     * @param registId 登记ID
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean del(Long registId) {
        // 根据登记ID和值班人员查询登记信息
        VisitingRegist visitingRegist = new LambdaQueryChainWrapper<>(baseMapper)
                .eq(VisitingRegist::getRegistId, registId)
                .eq(VisitingRegist::getAdminId, StpUtil.getLoginIdAsLong())
                .one();
        if (ObjectUtils.isEmpty(visitingRegist)) {
            throw new ServiceException("不能删除其他管理员的登记信息·");
        }
        // 编辑来访登记
        return baseMapper.deleteById(registId) > 0;
    }
}
