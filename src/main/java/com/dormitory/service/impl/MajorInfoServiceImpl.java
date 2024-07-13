package com.dormitory.service.impl;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.conditions.query.LambdaQueryChainWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.dormitory.controller.dto.MajorInfoDTO;
import com.dormitory.controller.qry.MajorInfoQry;
import com.dormitory.controller.vo.MajorInfoVO;
import com.dormitory.entity.ClassesInfo;
import com.dormitory.entity.MajorInfo;
import com.dormitory.exception.ServiceException;
import com.dormitory.mapper.ClassesInfoMapper;
import com.dormitory.mapper.MajorInfoMapper;
import com.dormitory.service.MajorInfoService;
import com.dormitory.utils.CopyUtils;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * <p>
 * 专业信息表 服务实现类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Service
@RequiredArgsConstructor
public class MajorInfoServiceImpl extends ServiceImpl<MajorInfoMapper, MajorInfo> implements MajorInfoService {

    /**
     * 班级信息Mapper
     */
    private final ClassesInfoMapper classesInfoMapper;

    /**
     * 专业信息分页查询
     *
     * @param qry 查询Qry
     * @return IPage<MajorInfoVO>
     */
    @Override
    public IPage<MajorInfoVO> pageByQry(MajorInfoQry qry) {
        IPage<MajorInfo> pages = new Page<>(qry.getPage(), qry.getLimit());
        pages = new LambdaQueryChainWrapper<>(baseMapper)
                .like(StringUtils.isNotEmpty(qry.getMajorName()), MajorInfo::getMajorName, qry.getMajorName())
                .eq(ObjectUtils.isNotEmpty(qry.getCollegeId()), MajorInfo::getCollegeId, qry.getCollegeId())
                .page(pages);
        return CopyUtils.covertPage(pages, MajorInfoVO.class);
    }

    /**
     * 专业信息列表查询
     *
     * @param qry 查询Qry
     * @return List<MajorInfoVO>
     */
    @Override
    public List<MajorInfoVO> listByQry(MajorInfoQry qry) {
        List<MajorInfo> list = new LambdaQueryChainWrapper<>(baseMapper)
                .like(StringUtils.isNotEmpty(qry.getMajorName()), MajorInfo::getMajorName, qry.getMajorName())
                .list();
        return CopyUtils.classCopyList(list, MajorInfoVO.class);
    }

    /**
     * 专业信息详情
     *
     * @param majorId 专业ID
     * @return MajorInfoVO
     */
    @Override
    public MajorInfoVO detailById(Long majorId) {
        return CopyUtils.classCopy(baseMapper.selectById(majorId), MajorInfoVO.class);
    }

    /**
     * 新增专业信息
     *
     * @param dto 专业DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean add(MajorInfoDTO dto) {
        // 判断专业信息是否存在
        List<MajorInfo> list = new LambdaQueryChainWrapper<>(baseMapper)
                .eq(MajorInfo::getMajorName, dto.getMajorName())
                .eq(MajorInfo::getCollegeId, dto.getCollegeId())
                .list();
        if (!CollectionUtils.isEmpty(list)) {
            throw new ServiceException("专业信息已存在,新增失败!");
        }
        // 复制专业信息实体
        MajorInfo majorInfo = CopyUtils.classCopy(dto, MajorInfo.class);
        // 保存专业信息，并返回结果
        return baseMapper.insert(majorInfo) > 0;
    }

    /**
     * 编辑专业信息
     *
     * @param majorId 专业ID
     * @param dto     专业DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean edit(Long majorId, MajorInfoDTO dto) {
        // 判断专业信息是否存在
        List<MajorInfo> list = new LambdaQueryChainWrapper<>(baseMapper)
                .eq(MajorInfo::getMajorName, dto.getMajorName())
                .eq(MajorInfo::getCollegeId, dto.getCollegeId())
                .ne(MajorInfo::getMajorId, majorId)
                .list();
        if (!CollectionUtils.isEmpty(list)) {
            throw new ServiceException("专业信息已存在,编辑失败!");
        }
        // 复制专业信息实体
        MajorInfo majorInfo = CopyUtils.classCopy(dto, MajorInfo.class);
        // 设置专业ID
        majorInfo.setMajorId(majorId);
        // 编辑专业信息，并返回结果
        return baseMapper.updateById(majorInfo) > 0;
    }

    /**
     * 删除专业信息
     *
     * @param majorId 专业ID
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean del(Long majorId) {
        List<ClassesInfo> list = new LambdaQueryChainWrapper<>(classesInfoMapper)
                .eq(ClassesInfo::getMajorId, majorId)
                .list();
        if (!CollectionUtils.isEmpty(list)) {
            throw new ServiceException("该专业下存在使用的班级信息,删除失败!");
        }
        // 删除专业信息，并返回结果
        return baseMapper.deleteById(majorId) > 0;
    }
}
