package com.dormitory.service.impl;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.conditions.query.LambdaQueryChainWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.dormitory.controller.dto.CollegeInfoDTO;
import com.dormitory.controller.qry.CollegeInfoQry;
import com.dormitory.controller.vo.CollegeInfoVO;
import com.dormitory.entity.CollegeInfo;
import com.dormitory.entity.MajorInfo;
import com.dormitory.exception.ServiceException;
import com.dormitory.mapper.CollegeInfoMapper;
import com.dormitory.mapper.MajorInfoMapper;
import com.dormitory.service.CollegeInfoService;
import com.dormitory.utils.CopyUtils;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * <p>
 * 学院信息表 服务实现类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Service
@RequiredArgsConstructor
public class CollegeInfoServiceImpl extends ServiceImpl<CollegeInfoMapper, CollegeInfo> implements CollegeInfoService {

    /**
     * 专业信息Mapper
     */
    private final MajorInfoMapper majorInfoMapper;

    /**
     * 学院信息分页查询
     *
     * @param qry 查询Qry
     * @return IPage<CollegeInfoVO>
     */
    @Override
    public IPage<CollegeInfoVO> pageByQry(CollegeInfoQry qry) {
        IPage<CollegeInfo> pages = new Page<>(qry.getPage(), qry.getLimit());
        pages = new LambdaQueryChainWrapper<>(baseMapper)
                .like(StringUtils.isNotEmpty(qry.getCollegeName()), CollegeInfo::getCollegeName, qry.getCollegeName())
                .page(pages);
        return CopyUtils.covertPage(pages, CollegeInfoVO.class);
    }

    /**
     * 学院信息列表查询
     *
     * @param qry 查询Qry
     * @return List<CollegeInfoVO>
     */
    @Override
    public List<CollegeInfoVO> listByParam(CollegeInfoQry qry) {
        List<CollegeInfo> list = new LambdaQueryChainWrapper<>(baseMapper)
                .like(StringUtils.isNotEmpty(qry.getCollegeName()), CollegeInfo::getCollegeName, qry.getCollegeName())
                .list();
        return CopyUtils.classCopyList(list, CollegeInfoVO.class);
    }

    /**
     * 学院信息详情
     *
     * @param collegeId 学院ID
     * @return CollegeInfoVO
     */
    @Override
    public CollegeInfoVO detailById(Long collegeId) {
        return CopyUtils.classCopy(baseMapper.selectById(collegeId), CollegeInfoVO.class);
    }

    /**
     * 新增学院信息
     *
     * @param dto 学院DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean add(CollegeInfoDTO dto) {
        // 判断学院信息是否存在
        List<CollegeInfo> list = new LambdaQueryChainWrapper<>(baseMapper)
                .eq(CollegeInfo::getCollegeName, dto.getCollegeName())
                .list();
        if (!CollectionUtils.isEmpty(list)) {
            throw new ServiceException("学院信息已存在,新增失败!");
        }
        // 复制学院信息实体
        CollegeInfo collegeInfo = CopyUtils.classCopy(dto, CollegeInfo.class);
        // 保存学院信息，并返回结果
        return baseMapper.insert(collegeInfo) > 0;
    }

    /**
     * 编辑学院信息
     *
     * @param collegeId 学院ID
     * @param dto       学院DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean edit(Long collegeId, CollegeInfoDTO dto) {
        // 判断学院信息是否存在
        List<CollegeInfo> list = new LambdaQueryChainWrapper<>(baseMapper)
                .eq(CollegeInfo::getCollegeName, dto.getCollegeName())
                .ne(CollegeInfo::getCollegeId, collegeId)
                .list();
        if (!CollectionUtils.isEmpty(list)) {
            throw new ServiceException("学院信息已存在,编辑失败!");
        }
        // 复制学院信息实体
        CollegeInfo collegeInfo = CopyUtils.classCopy(dto, CollegeInfo.class);
        // 设置学院ID
        collegeInfo.setCollegeId(collegeId);
        // 编辑学院信息，并返回结果
        return baseMapper.updateById(collegeInfo) > 0;
    }

    /**
     * 删除学院信息
     *
     * @param collegeId 学院ID
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean del(Long collegeId) {
        List<MajorInfo> list = new LambdaQueryChainWrapper<>(majorInfoMapper)
                .eq(MajorInfo::getCollegeId, collegeId)
                .list();
        if (!CollectionUtils.isEmpty(list)) {
            throw new ServiceException("该学院下存在使用的专业信息,删除失败!");
        }
        // 删除学院信息，并返回结果
        return baseMapper.deleteById(collegeId) > 0;
    }
}
