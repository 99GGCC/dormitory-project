package com.dormitory.controller.web.student;


import com.dormitory.common.R;
import com.dormitory.controller.qry.MajorInfoQry;
import com.dormitory.controller.vo.MajorInfoVO;
import com.dormitory.service.MajorInfoService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * <p>
 * 专业信息表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/student/major/info")
@Api(value = "StudentMajorInfoController", tags = {"专业信息接口"})
public class StudentMajorInfoController {

    /**
     * 专业信息Service
     */
    private final MajorInfoService majorInfoService;

    @ApiOperation("专业信息列表查询")
    @GetMapping("/list")
    public R<List<MajorInfoVO>> listByQry(MajorInfoQry qry) {
        return R.success(majorInfoService.listByParam(qry));
    }

    @ApiOperation("专业信息详情")
    @GetMapping("/detail/{majorId}")
    public R<MajorInfoVO> detail(@PathVariable @NotNull(message = "专业ID") Long majorId) {
        return R.success(majorInfoService.detailById(majorId));
    }
}

